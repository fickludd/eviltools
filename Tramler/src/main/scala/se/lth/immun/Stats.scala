package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._


object Stats {
	val opString = "stats"
}

class Stats extends TramlOperation("STATS") {
	
	def setup(params:String, mods:Seq[IModifier]) = {}
	
	def usage:String = """Print statistics on the current TraML. No options.
"""

	def operate(in:GhostTraML):GhostTraML = {
		
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
			println("%d-%d transitions per compound/peptide".format(tgSizes.min, tgSizes.max))
			println(" transitions q1 mzs: %.3f-%.3f".format(transitionQ1s.min, transitionQ1s.max))
			println(" transitions q3 mzs: %.3f-%.3f".format(transitionQ3s.min, transitionQ3s.max))
			println("transitions charges: %d-%d".format(transitionCharges.min, transitionCharges.max))
		}
		
		return in 
	}
}