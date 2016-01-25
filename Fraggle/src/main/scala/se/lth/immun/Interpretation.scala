package se.lth.immun

import se.lth.immun.mzml.ghost.GhostSpectrum
import se.lth.immun.chem.Ion

import scala.collection.mutable.ArrayBuffer

import se.lth.immun.protocol._
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType
import se.lth.immun.chem.EPeptideFragment


object Interpretation {
	
	case class InternalIon(first:Int, last:Int, mz:Double, z:Int)
	type FragmentMzDescription = (Double, FragmentDescription)
	type FragmentDescription = Either[Ion[se.lth.immun.chem.PeptideFragment], InternalIon]
	
	trait DiffThreshold {
		def within(x:Double, x0:Double):Boolean
		def range(x:Double):(Double, Double)
	}
	case class AbsThreshold(dx:Double) extends DiffThreshold {
		def within(x:Double, x0:Double) = math.abs(x - x0) < dx
		def range(x:Double):(Double, Double) = (x-dx, x+dx)
	}
	case class PPMThreshold(ppm:Double) extends DiffThreshold {
		def within(x:Double, x0:Double) = 
			math.abs(x - x0) / x0 * 1e6 < ppm
		def range(x:Double):(Double, Double) = {
			val dx = ppm / 1e6 * x
			(x - dx, x + dx)
		}
	}
	
	
	case class MetaBuffer(startMz:Double, tMz:Double, endMz:Double, fdesc:FragmentDescription)
	abstract class FragSpecBuffer(val meta:MetaBuffer) {
		def push(mz:Double, intensity:Double)
		def compile:FragmentAnnotation
	}
	class Closest(meta:MetaBuffer) extends FragSpecBuffer(meta) {
		var bestMz = 0.0
		var bestIntensity = 0.0
		def push(mz:Double, intensity:Double) = {
			if (bestMz < meta.tMz && mz < 2*meta.tMz - bestMz) {
				bestMz = mz
				bestIntensity = intensity
			}
		}
		def compile = desc2annot(meta.fdesc, bestIntensity, meta.tMz, bestMz)
	}
	class Uniform(meta:MetaBuffer) extends FragSpecBuffer(meta) {
		val mzs = new ArrayBuffer[Double]
		val intensities = new ArrayBuffer[Double]
		def push(mz:Double, intensity:Double) = {
			mzs += mz
			intensities += intensity
		}
		def compile = 
			desc2annot(
				meta.fdesc, 
				if (mzs.nonEmpty) intensities.sum else 0.0, 
				meta.tMz, 
				if (mzs.nonEmpty) weightedMean(mzs, intensities) else 0.0
			)
		def weightedMean(x:Seq[Double], y:Seq[Double]):Double = {
			var m = 0.0
			var w = 0.0
			var i = 0
			while( i < x.length) {
				m += x(i) * y(i)
				w += y(i)
				i += 1
			}
			m / w
		}
	}

	
	def naive(
			gs:GhostSpectrum,
			sortedFragIons:Seq[FragmentMzDescription],
			fragThreshold:DiffThreshold
	) = {
		val fragIons = new ArrayBuffer[FragmentAnnotation]
		var i = 0
		var j = 0
		while (i < gs.mzs.length && j < sortedFragIons.length) {
			val (fmz, fdesc) = sortedFragIons(j)
			val mzDiff = gs.mzs(i) - fmz
			if (fragThreshold.within(gs.mzs(i), fmz)) {
				val mzErrPPM = math.abs(mzDiff) / fmz * 1e6 
				val fragIon = desc2annot(fdesc, gs.intensities(i), fmz, gs.mzs(i))
				fragIons += fragIon
				i += 1
				j += 1
			} else if (mzDiff < 0) {
				
				i += 1
			} else {
				
				j += 1
			}
		}
		fragIons
	}
	
	
	
	def advanced(
			makeBuffer:MetaBuffer => FragSpecBuffer
		)(
			gs:GhostSpectrum,
			sortedFragIons:Seq[FragmentMzDescription],
			fragThreshold:DiffThreshold
	) = {
		val fragIons = new ArrayBuffer[FragmentAnnotation]
		
		var buffers = List[FragSpecBuffer]()
		var i = 0
		var j = 0
		while (i < gs.mzs.length && j < sortedFragIons.length) {
			val (fmz, fdesc) = sortedFragIons(j)
			val (fmzMin, fmzMax) = fragThreshold.range(fmz)
			val mzDiff = gs.mzs(i) - fmzMin
			val currMz = math.min(gs.mzs(i), fmzMin)
			if (mzDiff < 0) {// the theoretical fragment is next
				buffers = buffers :+ new Closest(MetaBuffer(fmzMin, fmz, fmzMax, fdesc))
				j += 1
			} else { // the empirical peak is next 
				for (b <- buffers) b.push(gs.mzs(i), gs.intensities(i))
				i += 1
			}
			
			val (done, valid) = buffers.partition(_.meta.endMz < currMz)
			fragIons ++= done.map(_.compile)
			buffers = valid
		}
		
		while (i < gs.mzs.length) {
			for (b <- buffers) b.push(gs.mzs(i), gs.intensities(i))
			val (done, valid) = buffers.partition(_.meta.endMz < gs.mzs(i))
			fragIons ++= done.map(_.compile)
			buffers = valid
			i += 1
		}
		
		fragIons ++= buffers.map(_.compile).filter(_.base.intensity > 0)
		
		fragIons
	}
	
	
	def desc2annot(fdesc:FragmentDescription, intensity:Double, theoreticalMz:Double, empiricalMz:Double) = {
		val mzErrPPM = math.abs(theoreticalMz - theoreticalMz) / theoreticalMz * 1e6 
		fdesc match {
			case Left(fi) =>
				SimpleFragment(
					BaseFragment(
						intensity, 
						fi.numExtraProtons - fi.numExtraElectrons, 
						Some(theoreticalMz),
						None,
						Some(mzErrPPM),
						1
					),
					toFragType(fi.molecule.fragmentType),
					fi.molecule.ordinal
				)
			case Right(ii) => 
				InternalFragment(
					BaseFragment(
						intensity, 
						ii.z, 
						Some(theoreticalMz),
						None,
						Some(mzErrPPM),
						1
					),
					ii.first, 
					ii.last
				)
		}
	}
	
	
	
	def toFragType(t:EPeptideFragment):FragmentType = 
		t match {
			case EPeptideFragment.a => FragmentType.A
			case EPeptideFragment.b => FragmentType.B
			case EPeptideFragment.c => FragmentType.C
			case EPeptideFragment.x => FragmentType.X
			case EPeptideFragment.y => FragmentType.Y
			case EPeptideFragment.z => FragmentType.Z
		}
}