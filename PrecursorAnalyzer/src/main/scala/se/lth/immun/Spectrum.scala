package se.lth.immun

import se.lth.immun.mzml.ghost._
import scala.collection.mutable.ArrayBuffer

object Spectrum {

	val INJECTION_TIME_ACC = "MS:1000927"
	val SCAN_START_TIME_ACC = "MS:1000016"
		
		
	case class Precursor(low:Double, high:Double)
	
	def parse(s:se.lth.immun.mzml.Spectrum, gs:GhostSpectrum):Spectrum = {
		
		import Ghost._
		
		val x = new Spectrum
		
		x.index = s.index
		for {
			scanList <- s.scanList
			cv <- scanList.scans.head.cvParams 
		} cv.accession match {
			case INJECTION_TIME_ACC => x.injTime = cv.value.get.toDouble
			case SCAN_START_TIME_ACC => x.scanStartTime = cv.value.get.toDouble
			case _ => {}
		}
		x.ints = gs.intensities
		x.mzs = gs.mzs
		x.tic = x.ints.sum
		
		
		for {
			p <- s.precursors
			iw <- p.isolationWindow
		} {
			var q1 = 0.0
			var q1up = 0.0
			var q1low = 0.0
			
			for (cv <- iw.cvParams) cv.accession match {
				case ISOLATION_WINDOW_TARGET => q1 = cv.value.get.toDouble
				case ISOLATION_WINDOW_LOWER_OFFSET => q1low = cv.value.get.toDouble
				case ISOLATION_WINDOW_UPPER_OFFSET => q1up = cv.value.get.toDouble
			}
			
			x.precursors += Precursor(q1-q1low, q1+q1up)
		}
		
		x
	}
}

class Spectrum {
	
	import Spectrum._
	
	var injTime:Double = _
	var scanStartTime:Double = _
	var index:Int = _
	var precursors = new ArrayBuffer[Precursor]
	var msLevel:Int = _
	
	var ints:Seq[Double] = _
	var mzs:Seq[Double] = _
	var tic:Double = _
	
	def area(mz0:Double, mz1:Double) = {
		var i = 0
		var area = 0.0
		while (i < mzs.length && mzs(i) < mz1) {
			if (mzs(i) >= mz0)
				area += ints(i)
			i += 1
		}
		area
	}
}