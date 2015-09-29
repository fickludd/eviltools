package se.lth.immun

import FraggleIntermediary._
import se.lth.immun.protocol.AAMolecule

object BestMerger {

	
	def merge(b:AAMoleculeBuilder):AAMolecule = 
		AAMolecule(
				0,
				b.sequence,
				b.mass,
				b.observations.map(ob =>
					ob.xs.maxBy(_.fragBaseIntensity)
				))
}