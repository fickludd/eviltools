package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import scala.collection.mutable.HashSet

import Range._
		
object Trim extends TramlOperation.Generator(
		"trim",
		"""  Filter targets and transition groups (TGs) based on different properties.
    precursorMz       LOW:HIGH mz range filter
    precursorMass     LOW:HIGH mass range filter
    precursorCharge   LOW:HIGH charge (integer) range filter
    fragmentMz        LOW:HIGH mz range filter
    fragmentMass      LOW:HIGH mass range filter
    fragmentCharge    LOW:HIGH charge (integer) range filter
    nFragments        LOW:HIGH integer range. Will discard TGs with less than LOW
                      transitions. The HIGH most instense transition will be used 
                      for TGs with more than HIGH transitions.  
    nTermMinOrdinal   The minimal ordinal to include for n-terminal fragments
    cTermMinOrdinal   The minimal ordinal to include for c-terminal fragments
    internalMinSize   The minimal length to include for internal fragments
    fragmentTypes     FTYPE;FTYPE;...FTYPE - fragment types are a,b,c,x,y,z and m (internal) and u (unknown)
    onlyNote          No filtering will be done, but targets and transitions will be annotated 
                      with eventual reasons for exclusion.
    diaRule           Remove fragments with m/z in the DIA MS2 isolation window"""
) {

	trait FragmentType 
	case class CTerm(ftype:Char, ord:Int) extends FragmentType
	case class NTerm(ftype:Char, ord:Int) extends FragmentType
	case class Internal(first:Int, last:Int) extends FragmentType
	case class Unknown(str:String) extends FragmentType
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new TrimInstance(params, mods)
	
	class TrimInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var precMzRange 	= DRange(-1e6, 1e6)
		var precMassRange 	= DRange(-1e6, 1e6)
		var precZRange 		= IRange(-1000, 1000)
		var fragMzRange 	= DRange(-1e6, 1e6)
		var fragMassRange 	= DRange(-1e6, 1e6)
		var fragZRange 		= IRange(-1000, 1000)
		var nTargetRange 	= IRange(0, 1000)
		var nFragRange 		= IRange(0, 1000)
		var nTermMinOrdinal:Int = 0
		var cTermMinOrdinal:Int = 0
		var internalMinSize:Int = 0
		var fragmentTypes = Array('a', 'b', 'c', 'x', 'y', 'z', 'm', 'u')
		var onlyNote 	= false
		var diaRule 	= false
		
		for ((k,v) <- params)
			k match {
				case "precursorMz" 		=> precMzRange = toDoubleRange(v)
				case "precursorMass" 	=> precMassRange = toDoubleRange(v)
				case "precursorCharge" 	=> precZRange = toIntRange(v)
				case "fragmentMz" 		=> fragMzRange = toDoubleRange(v)
				case "fragmentMass" 	=> fragMassRange = toDoubleRange(v)
				case "fragmentCharge" 	=> fragZRange = toIntRange(v)
				case "nMs1Targets" 		=> nTargetRange = toIntRange(v)
				case "nFragments" 		=> nFragRange = toIntRange(v)
				case "nTermMinOrdinal" 	=> nTermMinOrdinal = v.toInt
				case "cTermMinOrdinal" 	=> cTermMinOrdinal = v.toInt
				case "internalMinSize" 	=> internalMinSize = v.toInt
				case "fragmentTypes" 	=> fragmentTypes = v.split(";").map(_.head)
				case "onlyNote" 		=> onlyNote = true
				case "diaRule" 			=> diaRule = true
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			def precMzOk(mz:Double) =
				if (diaRule)
					params.diaWindows.exists(_.has(mz))
				else
					precMzRange.has(mz)
					
			def diaFragOk(precMz:Double, fragMz:Double) = 
				if (diaRule)
					params.diaWindows.find(_.has(precMz)) match {
						case Some(range) =>
							!range.has(fragMz)
						case None =>
							throw new Exception("BUG! Shouldn't consider transisions with precursors masses outside all dia windows! ")
					}
				else true
			
			val out = new ClearTraML
			
			val compounds =
				for (cp <- in.compounds) yield {
					val outCP = new ClearPeptide(cp.id, cp.proteins)
					outCP.rt = cp.rt
					
					val outAssays =
						for {
							a <- cp.assays
							if precZRange.has(a.z)
							if precMassRange.has((a.mz - Constants.PROTON_WEIGHT) * a.z)
							if precMzOk(a.mz)
						} yield {
							val outAssay = a.metaCopy
							outAssay.ms2Channels ++= 
								a.ms2Channels.filter(ch =>
									fragZRange.has(ch.z) &&
									fragMassRange.has((ch.mz - Constants.PROTON_WEIGHT)*ch.z) &&
									fragMzRange.has(ch.mz) &&
									ionOk(ch.id) &&
									diaFragOk(a.mz, a.z)
								).sortBy(- _.expIntensity.getOrElse(0.0)).take(nFragRange.high)
							outAssay.ms1Channels ++=
								a.ms1Channels.filter(ch =>
									precMassRange.has((ch.mz - Constants.PROTON_WEIGHT)/ch.z) &&
									precMzOk(ch.mz)
								).sortBy(- _.expIntensity.getOrElse(0.0)).take(nTargetRange.high)
								
							outAssay
						}
					
					outCP.assays ++= 
						outAssays.filter(a =>
							a.ms1Channels.length >= nTargetRange.low &&
							a.ms2Channels.length >= nFragRange.low
						)
					
					outCP
				}
			
			for (cp <- compounds.filter(_.assays.nonEmpty)) out.compounds += cp
			out.proteins ++= compounds.flatMap(_.proteins)
			
			out
		}
		
		
		def ionOk(ion:String) = {
			fragmentType(ion) match {
				case CTerm(ftype, ord) =>
					fragmentTypes.contains(ftype) && ord >= cTermMinOrdinal
				case NTerm(ftype, ord) =>
					fragmentTypes.contains(ftype) && ord >= nTermMinOrdinal
				case Internal(first, last) =>
					fragmentTypes.contains('m') && (last - first + 1) >= internalMinSize
				case Unknown(str) =>
					fragmentTypes.contains('u')
			}
		}
	}
	
	
	val regularFragmentRE = """([abcxyz])(\d+)""".r.unanchored
	val internalFragmentRE = """m(\d+):(\d+)""".r
	def fragmentType(ion:String):FragmentType = {
		def toRef(ionType:String, ordinal:String) =
			ionType match {
				case "a" => NTerm('a', ordinal.toInt)
				case "b" => NTerm('b', ordinal.toInt)
				case "c" => NTerm('c', ordinal.toInt)
				case "x" => CTerm('x', ordinal.toInt)
				case "y" => CTerm('y', ordinal.toInt)
				case "z" => CTerm('z', ordinal.toInt)
			}
		
		ion match {
			case regularFragmentRE(ionType, ordinal) 	=> toRef(ionType, ordinal)
			case internalFragmentRE(from, to) 			=> Internal(from.toInt, to.toInt)
			case _ => Unknown(ion)
		}
	}
}