package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._
import se.lth.immun.xlink.XLink
import se.lth.immun.unimod.UniMod

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap


object Isotopes extends TramlOperation.Generator(
		"isotopes",
		"""  Create inclusion targets for the most frequent isotopes for peptides and compounds.
    chargeOffests    semi-colon separated list of charge offsets from the baseCharge that should be added
    mz               two semi-colon separated value of the minimum and maximum precursor m/z that should be included
    n                the number of isotopes to include per peptide-ion or compound-ion
    mode             the charge to give inclusion list ions. Either 
                       'guess'     which means that an educated guess 
	 	                           will be made based on the number 
                                   of K and R in the peptide sequence
                       'existing'  q1 charges present in existing 
		                           transitions will be used"""
) {
	case class NaturalOccurence(dm:Double, occurence:Double)
	
	trait ChargeMode
	case class Guess() extends ChargeMode
	case class Base(z:Int) extends ChargeMode
	case class Existing() extends ChargeMode
	
	val ISOTOPE_dM = Element.C13.monoisotopicWeight - Element.C.monoisotopicWeight
		
	import Isotopes._
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new IsotopesInstance(params, mods)
	
	class IsotopesInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var nIsotopes = 1
		var chargeMode:ChargeMode = Base(2)
		var chargeOffsets:Seq[Int] = List(0)
		var mzRange = (0.0, Double.MaxValue)
		
		for ((k,v) <- params)
			k match {
				case "n" => nIsotopes = v.toInt
				case "mode" => 
					chargeMode = 
						if (v == "guess") Guess()
						else if (v == "existing") Existing()
						else Base(v.toInt)
				case "chargeOffsets" =>
					chargeOffsets = v.split(";").map(_.trim.toInt)
				case "mz" =>
					val parts = v.split(":").map(_.trim.toDouble)
					mzRange = (parts(0), parts(1))
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			if (nIsotopes == 0)
				return in
			
			val out = in
			
			val failed = new ArrayBuffer[(GhostPeptide, Throwable)]
			
			lazy val existingQ1z = 
					in.transitions
						.map(gt => (gt.peptideRef, gt.q1z)).distinct
						.groupBy(_._1)
						.mapValues(_.map(_._2))
			
				
			
			import PeptideParser._
			for {
				(ref, pep) <- in.peptides
			} {
				val pepMol = PeptideParser.parseSequence(pep.sequence) match {
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
						
						val zs = 
							chargeMode match {
								case Guess() =>
									chargeOffsets.map(_ + guessCharge(m))
								case Base(z) => chargeOffsets.map(_ + z)
								case Existing() => existingQ1z.get(ref).getOrElse(chargeOffsets.map(_ + 2))
							}
							
						
						for {
							z <- zs
							occ <- occurs
						} {
							val mz = (m0 + occ.dm + Constants.PROTON_WEIGHT*z) / z
							if (mz >= mzRange._1 && mz < mzRange._2) {
								val gt = new GhostTarget
								gt.peptideRef = ref
								gt.id = "%s %dC13 +%d, naturally %.3f%%".format(gt.peptideRef, occ.dm.toInt, z, occ.occurence*100)
								gt.intensity = occ.occurence
								gt.q1 = mz
								gt.q1z = z
								in += gt
							}
						}
					case None => {}
				}
			}
			
			
			
			for ((key,comp) <- in.compounds) {
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
					gt.compoundRef = key
					gt.q1 = (m0 + occ.dm + Constants.PROTON_WEIGHT*z) / z
					gt.q1z = z
					gt.intensity = occ.occurence
					gt.id = "%s +%.1f, naturally %.3f%%".format(gt.compoundRef, occ.dm, occ.occurence*100)
					
					out += gt
				}
			}
			
			return out
		}
	}
		
		
		
	def getIsotopes(id:IsotopeDistribution, n:Int):Seq[NaturalOccurence] = {
		val isotopes = new ArrayBuffer[NaturalOccurence]
		for (i <- id.intensities.sorted.takeRight(n).reverse) {
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