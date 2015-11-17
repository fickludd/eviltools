package se.lth.immun

import FraggleIntermediary._
import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.Observation
import se.lth.immun.protocol.FragmentAnnotation
import se.lth.immun.protocol.BaseFragment
import se.lth.immun.protocol.SimpleFragment
import se.lth.immun.protocol.XLinkFragment
import se.lth.immun.protocol.InternalFragment

import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import org.apache.commons.math3.stat.StatUtils

trait Merger {
	
	def f(xs:Seq[Double]):Double

	def merge(b:AAMoleculeBuilder):AAMolecule = 
		AAMolecule(
				0,
				b.sequence,
				b.protein,
				b.mass,
				b.observations.map(ob =>
					Observation(
						ob.fragmentationType,
						ob.z,
						ob.ce,
						Some(f(ob.xs.flatMap(_.precursorMz))),
						Some(f(ob.xs.flatMap(_.precursorIntensity))),
						Some(f(ob.xs.flatMap(_.iRT))),
						Some(math.sqrt(StatUtils.variance(ob.xs.flatMap(_.iRT).toArray))),
						Some(ob.xs.flatMap(_.fragBaseIntensity).sum),
						Some(ob.xs.flatMap(_.qValue).max),
						Some(f(ob.xs.flatMap(_.percentAnnotatedOfMS2tic))),
						Some(ob.xs.length),
						None,
						None,
						Some(f(ob.xs.flatMap(_.precursorFeatureApexIntensity))),
						mergeObservations(ob.xs)
					)))
					
	def mergeObservations(fobses:Seq[Observation]):Seq[FragmentAnnotation] = {
		val pivot = new HashMap[FragKey, ArrayBuffer[FragmentAnnotation]]
		
		for {
			fobs <- fobses
			f <- fobs.fragments
		} {
			val key =
				f match {
					case sf:SimpleFragment =>
						FragKey(sf.base.z, sf.fragmentType, sf.ordinal, -1)
					case xf:XLinkFragment =>
						FragKey(xf.base.z, xf.fragmentType, xf.ordinal, xf.peptide)
					case i:InternalFragment =>
						FragKey(i.base.z, FragmentType.M, i.firstIndex, i.lastIndex)
				}
			
			if (!pivot.contains(key))
				pivot += key -> new ArrayBuffer
				
			pivot(key) += f
		}
		
		for ((key, fs) <- pivot.toSeq) yield {
			val intensities = fs.map(_.base.intensity).toArray
			val intMid = f(intensities)
			val intStd = StatUtils.variance(intensities)
			val mzErrPPMavg = StatUtils.mean(fs.flatMap(_.base.mzErrPPM).toArray)
			val base = BaseFragment(intMid, fs.head.base.z, fs.head.base.mz, Some(intStd), Some(mzErrPPMavg), fs.length)
			fs.head match {
				case sf:SimpleFragment 	=> SimpleFragment(base, sf.fragmentType, sf.ordinal)
				case xf:XLinkFragment 	=> XLinkFragment(base, xf.fragmentType, xf.ordinal, xf.peptide)
				case i:InternalFragment => InternalFragment(base, i.firstIndex, i.lastIndex)
			}
		}
	}

}