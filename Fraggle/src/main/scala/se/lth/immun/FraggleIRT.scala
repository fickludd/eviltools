package se.lth.immun

import FraggleIntermediary._
import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.Observation
import se.lth.immun.IRT._

object FraggleIRT {
	
	def map(predict:Double => Double)(x:RawAAMolecule):AAMolecule = 
		AAMolecule(
				0,
				x.sequence,
				x.protein,
				x.mass,
				x.observations.map(o => Observation(
						o.x.fragmentationType,
						o.x.z,
						o.x.ce,
						o.x.precursorMz,
						o.x.precursorIntensity,
						Some(predict(o.rt)),
						None,
						o.x.fragBaseIntensity,
						o.x.qValue,
						o.x.percentAnnotatedOfMS2tic,
						o.x.n,
						o.x.precursorType,
						o.x.precursorIntensityRank,
						o.x.precursorFeatureApexIntensity,
						o.x.score,
						o.x.fragments
				))
			)
		
	
	
	
	def findDataPoints(
			irtPeps:Seq[IRTPeptide], 
			aaMolecules:Seq[RawAAMolecule]
	):Seq[IRTDataPoint] = {
		for {
			irtPep <- irtPeps
			aaMol <- aaMolecules.filter(_.sequence == irtPep.sequence)
			obs <- aaMol.observations
		} yield 
			IRTDataPoint(
					irtPep.sequence, 
					irtPep.iRT, 
					obs.rt, 
					obs.x.precursorFeatureApexIntensity
						.orElse(obs.x.precursorIntensity)
						.getOrElse(1.0)
				)
	}
}