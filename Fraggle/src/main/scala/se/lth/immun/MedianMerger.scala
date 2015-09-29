package se.lth.immun

import org.apache.commons.math3.stat.StatUtils

object MedianMerger extends Merger {

	def f(xs:Seq[Double]) = 
		StatUtils.percentile(xs.toArray, 50.0)
}