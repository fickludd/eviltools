package se.lth.immun

import java.io.File

import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.SimpleFragment
import se.lth.immun.protocol.InternalFragment
import se.lth.immun.protocol.XLinkFragment
import se.lth.immun.protocol.Observation
import se.lth.immun.protocol.FragmentAnnotation
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType
import se.lth.immun.protocol.MSFragmentationProtocol.PrecursorType

object FragmentTsv {

	def write(f:File, aaMolecules:Seq[AAMolecule], nFrags:Int) = {
		
		val w = new TsvWriter(f)
		
		w.row(
			Array(
				"sequence", "mass", 
				"fragmentationType", "ce", "precursorCharge", "iRT", 
				"precursorMz", "precursorIntensity", "precursorIntensityRank", "precursorType",
				"fragmentIntensity", "fragmentCharge", "fragmentMz", "fragmentIntensityStd", "fragmentMzErrPPM",
				"fragmentType", "ordinal", "firstInternal", "lastInternal", "xlinkPeptide"
			).toSeq)
		
		def filterFrags(frags:Seq[FragmentAnnotation]) = 
			if (nFrags <= 0) frags
			else frags.sortBy(-_.base.intensity).take(nFrags).sortBy(_.base.mz)
			
		for {
			aaMol <- aaMolecules
			obs <- aaMol.observations
			frag <- filterFrags(obs.fragments)
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
			w.row(Array(
				aaMol.sequence,
				aaMol.mass,
				obs.fragmentationType,
				obs.ce,
				obs.z,
				obs.iRT.getOrElse(Double.NaN),
				obs.precursorMz.getOrElse(0.0),
				obs.precursorIntensity.getOrElse(0.0),
				obs.precursorIntensityRank.getOrElse(1),
				obs.precursorType.getOrElse(PrecursorType.ORIG),
				frag.base.intensity,
				frag.base.z,
				frag.base.mz.getOrElse(Double.NaN),
				frag.base.intensityStd.getOrElse(Double.NaN),
				frag.base.mzErrPPM.getOrElse(Double.NaN)
				) ++ fragCols)
		}
		
		w.close
	}
}