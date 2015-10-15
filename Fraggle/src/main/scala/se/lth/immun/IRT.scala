package se.lth.immun

import java.io.File
import scala.io.Source

import FraggleIntermediary._
import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.Observation

import org.apache.commons.math3.stat.regression.SimpleRegression

object IRT {

	case class IRTPeptide(sequence:String, iRT:Double)
	case class IRTDataPoint(sequence:String, iRT:Double, rt:Double)
	
	class IRTMap(
			val slope:Double, 
			val intercept:Double, 
			val r2:Double, 
			val residualStd:Double, 
			val dataPoints:Seq[IRTDataPoint]
	) extends Function1[RawAAMolecule, AAMolecule]{
		def apply(x:RawAAMolecule):AAMolecule = 
			AAMolecule(
					0,
					x.sequence,
					x.protein,
					x.mass,
					x.observations.map(o => Observation(
							o.fragmentationType,
							o.z,
							o.ce,
							o.precursorMz,
							o.precursorIntensity,
							o.rt * slope + intercept,
							o.fragBaseIntensity,
							o.qValue,
							o.percentAnnotatedOfMS2tic,
							o.n,
							o.fragments
					))
				)
				
		override def toString = 
			"IRTMap(slope=%f, intercept=%f, r2=%f, resStd=%f, nDataPoints=%d)".format(slope, intercept, r2, residualStd, dataPoints.length)
	}
	
	def readPeptideTsv(f:File):Seq[IRTPeptide] = {
		
		(for ( line <- Source.fromFile(f).getLines.drop(1) ) yield {
			val p = line.split("\t")
			IRTPeptide(p(0), p(1).toDouble)
		}).toSeq
	}
	
	def findDataPoints(
			irtPeps:Seq[IRTPeptide], 
			aaMolecules:Seq[RawAAMolecule]
	):Seq[IRTDataPoint] = {
		for {
			irtPep <- irtPeps
			aaMol <- aaMolecules.filter(_.sequence == irtPep.sequence)
			obs <- aaMol.observations
		} yield IRTDataPoint(irtPep.sequence, irtPep.iRT, obs.rt)
	}
	
	def createMap(dataPoints:Seq[IRTDataPoint]):IRTMap = {
		val sr = new SimpleRegression(true)
		for (dp <- dataPoints)
			sr.addData(dp.rt, dp.iRT)
		val rr = sr.regress
		new IRTMap(
				sr.getSlope,
				sr.getIntercept,
				rr.getRSquared,
				math.sqrt(rr.getMeanSquareError),
				dataPoints
			)
		
	}
}