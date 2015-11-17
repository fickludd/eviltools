package se.lth.immun

import java.io.File

import se.lth.immun.protocol.AAMolecule

object ObservationTsv {

	def write(f:File, aaMolecules:Seq[AAMolecule]) = {
		
		val w = new TsvWriter(f)
		
		w.row("protein", "aaMolecule", "z", "score")
		
		for {
			aaMol <- aaMolecules
			obs <- aaMol.observations
		} {
			w.row(aaMol.protein, aaMol.sequence, obs.z, obs)
		}
		
		w.close
	}
}