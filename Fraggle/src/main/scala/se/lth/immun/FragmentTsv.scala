package se.lth.immun

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.SimpleFragment
import se.lth.immun.protocol.InternalFragment
import se.lth.immun.protocol.XLinkFragment
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType

object FragmentTsv {

	def write(f:File, aaMolecules:Seq[AAMolecule]) = {
		
		val w = new BufferedWriter(new FileWriter(f))
		
		def row(a:Any*)(b:Array[Any]) =
			w.write((a++b).mkString("\t") + "\n")
		
		row(
			"sequence", "mass", 
			"fragmentationType", "ce", "precursorCharge", "iRT", "precursorMz", "precursorIntensity", 
			"fragmentIntensity", "fragmentCharge", "fragmentMz", "fragmentIntensityStd", "fragmentMzErrPPM" 
			)(Array("fragmentType", "ordinal", "firstInternal", "lastInternal", "xlinkPeptide"))
			
		for {
			aaMol <- aaMolecules
			obs <- aaMol.observations
			frag <- obs.fragments
		} {
			val fragCols = 
				frag match {
					case SimpleFragment(base, ftype, ordinal) =>
						Array(ftype, ordinal, 0, 0, -1)
					case XLinkFragment(base, ftype, ordinal, peptide) =>
						Array(ftype, ordinal, 0, 0, peptide)
					case InternalFragment(base, first, last) =>
						Array(FragmentType.M, 0, first, last, -1)
				}
			row(
				aaMol.sequence,
				aaMol.mass,
				obs.fragmentationType,
				obs.ce,
				obs.z,
				obs.iRT,
				obs.precursorMz,
				obs.precursorIntensity,
				frag.base.intensity,
				frag.base.z,
				frag.base.mz.getOrElse(Double.NaN),
				frag.base.intensityStd.getOrElse(Double.NaN),
				frag.base.mzErrPPM.getOrElse(Double.NaN)
				)(fragCols)
		}
	}
}