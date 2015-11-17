package se.lth.immun

import se.lth.immun.chem._
import se.lth.immun.traml.clear._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import se.lth.immun.unimod.UniModEntry

object QualityControl {
	
	val CARBAMYL = UniModEntry.Carbamyl.modification
	val CARBAMYL_MASS = CARBAMYL.monoisotopicMass

	def check(in:ClearTraML, params:TramlerParams):(ClearTraML, Seq[String]) = {
		
		val out = new ClearTraML
		
		val abnormalities = new ArrayBuffer[String]
		def approve(cp:ClearPeptide) = {
			out.compounds += cp
			out.proteins ++= cp.proteins
		}
		
		import PeptideParser._
		for (cp <- in.compounds) {
			val outCP = new ClearPeptide(cp.id, cp.proteins)
			outCP.rt = cp.rt
			
			val mOpt = 
				parseSequence(cp.id) match {
					case XLinkPeptide(xl) => 
						Some(xl.monoisotopicMass)
					case UniModPeptide(p) => 
						Some(p.monoisotopicMass)
					case Unparsable(seq) =>
						abnormalities += "Cannot parse peptide '%s'!".format(seq)
						None 
			}
			
			val fixedCompounds = new HashMap[String, ClearPeptide]
			
			mOpt match {
				case None =>
					approve(cp)
				case Some(m) =>
					for (a <- cp.assays) {
						val inMass = (a.mz - Constants.PROTON_WEIGHT) * a.z
						val diff = inMass - m
						def reportAbnormMass =
							abnormalities += "Input precursor mass %9.5f differs from calculated %9.5f by %9.5f for z=%d ion of %s".format(inMass, m, diff, a.z, cp.id)
						if (diffWithinPPM(diff, m, params.qcMassDiffPPM))
							outCP.assays += a
						else
							if (params.attemptKnownFixes) {
								if (diffWithinPPM(diff - CARBAMYL_MASS, m - CARBAMYL_MASS, params.qcMassDiffPPM)) {
									val fixedSeq = "(%s)%s".format(CARBAMYL.toString, cp.id)
									if (!fixedCompounds.contains(fixedSeq)) {
										val fcp = new ClearPeptide(fixedSeq, cp.proteins)
										fcp.rt = cp.rt
										fixedCompounds += fixedSeq -> fcp
									}
									abnormalities += "Added missing Carbamylation: %s -> %s, z=%d".format(cp.id, fixedSeq, a.z)
									val fixedCp = fixedCompounds(fixedSeq)
									fixedCp.assays += a
								} else
									reportAbnormMass
							} else
								reportAbnormMass
					}
					if (outCP.assays.nonEmpty)
						approve(outCP)
					else
						abnormalities += "No valid assays for compound '%s'. Removing.".format(cp.id)
			}
			
			for (cp <- fixedCompounds.values)
				approve(cp)
		}
		
		(out, abnormalities)
	}
	
	def diffWithinPPM(diff:Double, m:Double, ppm:Double) = {
		math.abs(diff) / m * 1e6 < ppm
	}
}