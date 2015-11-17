package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._


object Stats extends TramlOperation.Generator(
		"stats",
		"""  Print statistics on the current TraML. No options."""
) {
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new StatsInstance(params)
	
	class StatsInstance(
			params:Seq[(String, String)]
	) extends TramlOperation.Instance(opString, params) {
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			//println("%6d compounds".format(in.compounds.size))
			val uniquePeps = in.compounds.map(_.id).toSet
			val assays = in.compounds.flatMap(_.assays)
			val nMs1Channels = assays.map(_.ms1Channels.length).sum
			val nMs2Channels = assays.map(_.ms2Channels.length).sum
			val rts = in.compounds.map(_.rt)
			println("%6d peptides".format(uniquePeps.size))
			println("%6d assays (pep ions)".format(assays.length))
			println("%6d ms1 channels".format(nMs1Channels))
			println("%6d ms2 channels".format(nMs2Channels))
			println
			
			println("           num peptides w. iRT: %8d".format(
					rts.count(_.collect({ case ClearRetentionTime.IRT(t) => true }).getOrElse(false))
				))
			println("                 normalized RT: %8d".format(
					rts.count(_.collect({ case x:ClearRetentionTime.Ntime => true }).getOrElse(false))
				))
			println("                   absolute RT: %8d".format(
					rts.count(_.collect({ case x:ClearRetentionTime.AbsoluteRT => true }).getOrElse(false))
				))
			println
			
			if (nMs1Channels > 0) {
				val sizes = assays.map(_.ms1Channels.length)
				val ms1Assays = assays.filter(_.ms1Channels.length > 0)
				val targetMzs = ms1Assays.flatMap(_.ms1Channels.map(_.mz))
				val targetCharges = ms1Assays.flatMap(_.ms1Channels.map(_.z))
				val tIntensities = ms1Assays.flatMap(_.ms1Channels.map(_.expIntensity))
				println(" ms1 channels per assay: %3d - %d".format(sizes.min, sizes.max))
				println("                charges: %3d - %d".format(targetCharges.min, targetCharges.max))
				println("                    mzs: %8.3f - %8.3f".format(targetMzs.min, targetMzs.max))
				println("           w. intensity: %7d / %7d".format(tIntensities.count(_.nonEmpty), nMs1Channels))
				println
			}
			
			if (nMs2Channels > 0) {
				val sizes = assays.map(_.ms2Channels.length)
				val ms2Assays = assays.filter(_.ms2Channels.length > 0)
				val tQ1s = ms2Assays.map(_.mz)
				val tCharges = ms2Assays.map(_.z)
				val tQ3s = ms2Assays.flatMap(_.ms2Channels.map(_.mz))
				val tIntensities = ms2Assays.flatMap(_.ms2Channels.map(_.expIntensity))
				println("  ms2 channels per assay: %3d - %d".format(sizes.min, sizes.max))
				println("             ms1 charges: %3d - %d".format(tCharges.min, tCharges.max))
				println("                 ms1 mzs: %8.3f - %8.3f".format(tQ1s.min, tQ1s.max))
				println("                 ms2 mzs: %8.3f - %8.3f".format(tQ3s.min, tQ3s.max))
				println("            w. intensity: %7d / %7d".format(tIntensities.count(_.nonEmpty), nMs2Channels))
				
				println
			}
			
			in 
		}
	}
}