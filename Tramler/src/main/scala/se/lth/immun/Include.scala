package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._
import se.lth.immun.xlink.XLink

object Include {
	val opString = "include"
}

class Include extends TramlOperation("INCLUDE") {
		
	var mods:Seq[IModifier] = Nil
	var baseCharge:Option[Int] = Some(2)
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
							case "baseCharge" =>
								baseCharge = 
									if (kv(1) == "guess") None
									else Some(kv(1).toInt)
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
	
	def usage:String = """THIS OPERATION IS DEPRECATED - USE 'ISOTOPES()' INSTEAD
		Create monoisotopic inclusion target for peptides and compounds. Options are
  baseCharge =     the charge to give inclusion list ions. Either an integer, or 'guess', which means that
                   an educated guess will be made based on the number of K and R in the peptide sequence
  chargeOffests =  semi-colon separated list of charge offsets from the baseCharge that should be added
  mz =             two semi-colon separated value of the minimum and maximum precursor m/z that should be included
"""
	
	def operate(in:GhostTraML):GhostTraML = {
		for {
			(ref, comp) <- in.compounds
			z <- 	if (comp.preferredCharges.nonEmpty) comp.preferredCharges 
					else chargeOffsets.map(_ + baseCharge.getOrElse(0))
		} {
			val gt = new GhostTarget
			gt.compoundRef = ref
			gt.id = ref
			gt.intensity = 1
			gt.q1 = (comp.mass + Constants.PROTON_WEIGHT*z) / z
			gt.q1z = z
			in += gt
		}
		
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
				case Some(m0) =>
					for (z <- chargeOffsets.map(_ + baseCharge.getOrElse(guessCharge(m0)))
					) {
						val mz = (m0.monoisotopicMass + Constants.PROTON_WEIGHT*z) / z
						if (mz >= mzRange._1 && mz < mzRange._2) {
							val gt = new GhostTarget
							gt.peptideRef = ref
							gt.id = "%s +%d".format(ref, z)
							gt.intensity = 1
							gt.q1 = mz
							gt.q1z = z
							in += gt
						}
					}
				case None => {}
			}
		}
		in
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