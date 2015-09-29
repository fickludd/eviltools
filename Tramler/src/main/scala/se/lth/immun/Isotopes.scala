package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._
import se.lth.immun.xlink.XLink
import se.lth.immun.unimod.UniMod

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ArrayBuffer


object Isotopes {
	case class NaturalOccurence(dm:Double, occurence:Double)
	
	trait ChargeMode
	case class Guess() extends ChargeMode
	case class Base(z:Int) extends ChargeMode
	case class Existing() extends ChargeMode
	
	val ISOTOPE_dM = Element.C13.monoisotopicWeight - Element.C.monoisotopicWeight
	
	val opString = "isotopes"
}

class Isotopes extends TramlOperation("ISOTOPES") {
		
	import Isotopes._
	
	var mods:Seq[IModifier] = Nil
	var nIsotopes = 1
	var chargeMode:ChargeMode = Base(2)
	var chargeOffsets:Seq[Int] = List(0)
	var mzRange = (0.0, Double.MaxValue)
	
	
	def setup(params:String, mods:Seq[IModifier]) = {
		this.mods = mods
		for (p <- params.split(","))
			p.trim match {
				case x =>
					if (x.contains("=")) {
						val kv = x.split("=")
						kv.head match {
							case "n" =>
								nIsotopes = x.drop(2).toInt
							case "mode" =>
								chargeMode = 
									if (kv(1) == "guess") Guess()
									else if (kv(1) == "existing") Existing()
									else Base(kv(1).toInt)
							case "chargeOffsets" =>
								chargeOffsets =
									kv(1).split(";").map(_.trim.toInt)
							case "mz" =>
								val parts = kv(1).split(";").map(_.trim.toDouble)
								mzRange = (parts(0), parts(1))
							case x =>
								throw new IllegalArgumentException("Unknown param '"+x+"'")
						}
					} else
						throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
	}
	
	def usage:String = """Create inclusion targets for the most frequent isotopes for peptides and compounds. Options are
  mode =     the charge to give inclusion list ions. Either  
             integer
             'guess'     which means that an educated guess will be made based on the number 
                         of K and R in the peptide sequence
             'existing'  q1 charges present in existing transitions will be used  
  chargeOffests =  semi-colon separated list of charge offsets from the baseCharge that should be added
  mz =             two semi-colon separated value of the minimum and maximum precursor m/z that should be included
  n=            the number of isotopes to include per peptide-ion or compound-ion
"""
	
	def operate(in:GhostTraML):GhostTraML = {
		if (nIsotopes == 0)
			return in
		
		val out = in
		
		val failed = new ArrayBuffer[(GhostPeptide, Throwable)]
		
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
					val occurs = getIsotopes(m.getIsotopeDistribution)
					val m0 = m.monoisotopicMass
					
					val zs = 
						chargeMode match {
							case Guess() =>
								chargeOffsets.map(_ + guessCharge(m))
							case Base(z) => chargeOffsets.map(_ + z)
							case Existing() =>
								in.transitions.filter(_.peptideRef == ref).map(_.q1z).toSet.toSeq
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
			val occurs = getIsotopes(ec.getIsotopeDistribution)
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
	
	
	
	def getIsotopes(id:IsotopeDistribution):Seq[NaturalOccurence] = {
		val isotopes = new ArrayBuffer[NaturalOccurence]
		for (i <- id.intensities.sorted.takeRight(nIsotopes).reverse) {
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