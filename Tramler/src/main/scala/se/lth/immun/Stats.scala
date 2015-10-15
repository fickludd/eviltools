package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._


object Stats extends TramlOperation.Generator(
		"stats",
		"""  Print statistics on the current TraML. No options."""
) {
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new StatsInstance(params)
	
	class StatsInstance(params:Seq[(String, String)]) extends TramlOperation.Instance(opString, params) {
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			
			println("%d compounds".format(in.compounds.size))
			println("%d peptides".format(in.peptides.size))
			println("%d targets".format(in.includes.length))
			println("%d transitions".format(in.transitions.length))
			println
			
			if (in.includes.nonEmpty) {
				val igSizes = in.includeGroups.map(_._2.length)
				val targetMzs = in.includes.map(_.q1)
				val targetCharges = in.includes.map(_.q1z)
				println("%d-%d targets per compound/peptide".format(igSizes.min, igSizes.max))
				println("    target mzs: %.3f-%.3f".format(targetMzs.min, targetMzs.max))
				println("target charges: %d-%d".format(targetCharges.min, targetCharges.max))
				println
			}
			
			if (in.transitions.nonEmpty) {
				val tgSizes = in.transitionGroups.map(_._2.length)
				val transitionQ1s = in.transitions.map(_.q1)
				val transitionQ3s = in.transitions.map(_.q3)
				val transitionCharges = in.transitions.map(_.q1z)
				println("%d-%d transitions per compound/peptide ion".format(tgSizes.min, tgSizes.max))
				println(" transitions q1 mzs: %.3f-%.3f".format(transitionQ1s.min, transitionQ1s.max))
				println(" transitions q3 mzs: %.3f-%.3f".format(transitionQ3s.min, transitionQ3s.max))
				println("transitions charges: %d-%d".format(transitionCharges.min, transitionCharges.max))
			}
			
			return in 
		}
	}
}