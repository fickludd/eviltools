package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._

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
    fragmentTypes     FTYPE;FTYPE;...FTYPE - fragment types are a,b,c,x,y,z and m (internal)
    onlyNote          No filtering will be done, but targets and transitions will be annotated 
                      with eventual reasons for exclusion.
    diaRule           Remove fragments with m/z in the DIA MS2 isolation window"""
) {

	trait FragmentType 
	case class CTerm(ftype:Char, ord:Int) extends FragmentType
	case class NTerm(ftype:Char, ord:Int) extends FragmentType
	case class Internal(first:Int, last:Int) extends FragmentType
	
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
		var nFragRange 		= IRange(0, 1000)
		var nTermMinOrdinal:Int = 0
		var cTermMinOrdinal:Int = 0
		var internalMinSize:Int = 0
		var fragmentTypes = Array('a', 'b', 'c', 'x', 'y', 'z', 'm')
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
		
		
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			
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
			
			def setupPeptideAndCompound(out:GhostTraML, pepRef:String, compRef:String) = {
				if (compRef != null && !out.compounds.contains(compRef))
					out.compounds += compRef -> in.compounds(compRef)
				if (pepRef != null && !out.peptides.contains(pepRef))
					out.peptides += pepRef -> in.peptides(pepRef)
			}
				
			val out = new GhostTraML
				
			for {
				t <- in.includes
				if precZRange.has(t.q1z)
				if precMassRange.has((t.q1 - Constants.PROTON_WEIGHT)/t.q1z)
				if precMzOk(t.q1)
			} {
				setupPeptideAndCompound(out, t.peptideRef, t.compoundRef)
				out += t
			}
			
			for {
				((mz, pep), gts) <- in.transitionGroups
				if precZRange.has(gts.head.q1z)
				if precMassRange.has((gts.head.q1 - Constants.PROTON_WEIGHT)/gts.head.q1z)
				if precMzOk(gts.head.q1)
			} {
				val okFrags =
					for {
						gt <- gts
						if fragZRange.has(gt.q3z)
						if fragMassRange.has((gt.q3 - Constants.PROTON_WEIGHT)/gt.q3z)
						if fragMzRange.has(gt.q3)
						if fragmentTypes.contains(gt.ions.head.head)
						if ionOk(gt.ions.head)
						if diaFragOk(gt.q1, gt.q3)
					} yield gt
					
				if (okFrags.length >= nFragRange.low) {
					setupPeptideAndCompound(out, pep, null)
					for (gt <- okFrags.sortBy(_.intensity).reverse.take(nFragRange.high))
						out += gt
				}
			}
						
			val protRefs = out.peptides.values.flatMap(_.proteins).toSet
			for (pr <- protRefs)
				out.proteins += pr -> in.proteins(pr)
			
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
			}
		}
	}
	
	
	def fragmentType(ion:String) = {
		ion.head match {
			case 'a' => NTerm(ion.head, ion.tail.toInt)
			case 'b' => NTerm(ion.head, ion.tail.toInt)
			case 'c' => NTerm(ion.head, ion.tail.toInt)
			case 'x' => CTerm(ion.head, ion.tail.toInt)
			case 'y' => CTerm(ion.head, ion.tail.toInt)
			case 'z' => CTerm(ion.head, ion.tail.toInt)
			case 'm' => 
				val FL = ion.tail.split(":", 2)
				Internal(FL(0).toInt, FL(1).toInt)
		}
	}
}