package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._
import se.lth.immun.xlink.XLink
import se.lth.immun.unimod.UniMod

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object Ms1Isotopes extends TramlOperation.Generator(
		"ms1-isotopes",
		"""  Create ms1 targets for the most frequent isotopes for peptides and compounds.
    chargeOffests    semi-colon separated list of charge offsets from the baseCharge that should be added
    mz               two semi-colon separated value of the minimum and maximum precursor m/z that should be included
    n                the number of isotopes to include per peptide-ion or compound-ion
    mode             the charge to give inclusion list ions. Either 
                       'guess'     which means that an educated guess 
                                   will be made based on the number 
                                   of K and R in the peptide sequence
                       'existing'  q1 charges present in existing 
                                   transitions will be used (default)
                       int         will assume int a base charge everywhere"""
) {
	case class NaturalOccurence(dm:Double, occurence:Double)
	
	trait ChargeMode
	case object Guess extends ChargeMode
	case class Base(z:Int) extends ChargeMode
	case object Existing extends ChargeMode
	
	val ISOTOPE_dM = Element.C13.monoisotopicWeight - Element.C.monoisotopicWeight
		
	import Ms1Isotopes._
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new Ms1IsotopesInstance(params, mods)
	
	class Ms1IsotopesInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var nIsotopes = 1
		var chargeMode:ChargeMode = Existing
		var chargeOffsets:Seq[Int] = List(0)
		var mzRange = (0.0, Double.MaxValue)
		
		for ((k,v) <- params)
			k match {
				case "n" => nIsotopes = v.toInt
				case "mode" => 
					chargeMode = 
						if (v == "guess") Guess
						else if (v == "existing") Existing
						else Base(v.toInt)
				case "chargeOffsets" =>
					chargeOffsets = v.split(";").map(_.trim.toInt)
				case "mz" =>
					val parts = v.split(":").map(_.trim.toDouble)
					mzRange = (parts(0), parts(1))
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			if (nIsotopes == 0)
				return in
			
			val out = in
				
			import PeptideParser._
			
			for (cp <- out.compounds) {
				val pepMol = PeptideParser.parseSequence(cp.id) match {
	    			case XLinkPeptide(xl) =>
	    				Some(xl)
	    			case UniModPeptide(p) =>
	    				Some(p)
	    			case Unparsable(seq) =>
	    				None
	    		}
				
				pepMol match {
					case Some(m) =>
						val occurs = getIsotopes(m.getIsotopeDistribution, nIsotopes)
						val m0 = m.monoisotopicMass
						
						chargeMode match {
							case Guess =>
								ensureAssaysForCharges(chargeOffsets.map(_ + guessCharge(m)), cp, m0)
							case Base(z) => 
								ensureAssaysForCharges(chargeOffsets.map(_ + z), cp, m0)
							case Existing => {}
						}
						
						for {
							a <- cp.assays
							occ <- occurs
						} {
							val mz = (m0 + occ.dm) / a.z + Constants.PROTON_WEIGHT
							a.ms1Channels += 
								Clear.Channel(
									mz,
									a.z,
									"%dC13, naturally %.3f%%".format(occ.dm.toInt, occ.occurence*100),
									1,
									Some(occ.occurence)
								)
						}
					case None => 
						println("Error parsing sequence "+cp.id)
				}
			}
			
			
			/*
			for ((_,comp) <- in.compounds) {
				val ec = Peptide.averagine(comp.mass)
				val occurs = getIsotopes(ec.getIsotopeDistribution, nIsotopes)
				val m0 = comp.mass
				
				val zs = 
					chargeMode match {
						case Guess() =>
							chargeOffsets.map(_ + 0)
						case Base(z) => chargeOffsets.map(_ + z)
						case Existing() =>
							comp.preferredCharges 
					}
							
				for {
					z <- zs
					occ <- occurs
				} {
					val gt = new GhostTarget
					gt.compound = Some(comp)
					gt.q1 = (m0 + occ.dm + Constants.PROTON_WEIGHT*z) / z
					gt.q1z = z
					gt.intensity = Some(occ.occurence)
					gt.id = "%s +%.1f, naturally %.3f%%".format(comp.id, occ.dm, occ.occurence*100)
					
					out += gt
				}
			}
			*/
			return out
		}
	}
	
	
	
	def ensureAssaysForCharges(
			zs:Seq[Int], 
			cp:ClearPeptide, 
			m:Double
	):Unit = {
		for (z <- zs)
			cp.assays.find(_.z == z) match {
				case Some(a) => {}
				case None => 
					val mz = m / z + Constants.PROTON_WEIGHT
					cp.getAssay(mz, z, None)
			}
	}
		
		
		
	def getIsotopes(id:IsotopeDistribution, n:Int):Seq[NaturalOccurence] = {
		val isotopes = new ArrayBuffer[NaturalOccurence]
		for (i <- id.intensities.sortBy(- _).take(n)) {
			val ii = id.intensities.indexOf(i)
			isotopes += NaturalOccurence(ISOTOPE_dM * ii, i)
		}
		isotopes
	}

	
	
	def guessCharge(m:IMolecule):Int = {
		def optGuessCharge(op:Option[Peptide]):Int =
			op.map(guessCharge).getOrElse(0)
			
		def guessCharge(p:Peptide):Int =
			1 + p.aminoAcids.tail.count(aa => aa == StandardAminoAcid.K || aa == StandardAminoAcid.R)
		
		m match {
			case xl:XLink =>
				val xlinkSub:Int = if (xl.pep2.isDefined) -2 else 0
				math.max(1, 
						guessCharge(xl.pep1) + optGuessCharge(xl.pep2) + xlinkSub
					)
			case p:Peptide =>
				math.max(1, guessCharge(p))
			case _ =>
				0
		}
	}
}