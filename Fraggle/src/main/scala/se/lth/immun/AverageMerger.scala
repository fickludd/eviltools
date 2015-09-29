package se.lth.immun

import org.apache.commons.math3.stat.StatUtils

object AverageMerger extends Merger {
	
	def f(xs:Seq[Double]) = 
		StatUtils.mean(xs.toArray)

}