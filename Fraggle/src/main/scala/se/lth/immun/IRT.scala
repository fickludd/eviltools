package se.lth.immun

import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}

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
							o.x.fragmentationType,
							o.x.z,
							o.x.ce,
							o.x.precursorMz,
							o.x.precursorIntensity,
							Some(o.rt * slope + intercept),
							o.x.fragBaseIntensity,
							o.x.qValue,
							o.x.percentAnnotatedOfMS2tic,
							o.x.n,
							o.x.precursorType,
							o.x.precursorIntensityRank,
							o.x.precursorFeatureApexIntensity,
							o.x.fragments
					))
				)
				
		override def toString = 
			"IRTMap(slope=%f, intercept=%f, r2=%f, resStd=%f, nDataPoints=%d)".format(slope, intercept, r2, residualStd, dataPoints.length)
	}
	
	
	
	def readPeptideTsv(f:File):Seq[IRTPeptide] = {
		
		var iPEPTIDE = -1
		var iIRT = -1
		
		var headerParsed = false
		val irtPeps = new ArrayBuffer[IRTPeptide]
		
		for ( line <- Source.fromFile(f).getLines ) {
			val p = line.split("\t")
			
			if (!headerParsed) {
				val lc = p.map(_.toLowerCase) 
				if (lc.contains("peptide") && lc.contains("irt")) {
					iPEPTIDE = lc.indexOf("peptide")
					iIRT = lc.indexOf("irt")
				} else {
					iPEPTIDE = p.indexWhere(x => Try(x.toDouble).isFailure)
					iIRT = p.indexWhere(x => Try(x.toDouble).isSuccess)
					if (iPEPTIDE < 0 || iIRT < 0)
						throw new Exception("Could not parse IRT peptide definition file. Needs peptide sequence and IRT columns!")
					irtPeps += IRTPeptide(p(iPEPTIDE), p(iIRT).toDouble)
				}
				headerParsed = true
			} else
				irtPeps += IRTPeptide(p(iPEPTIDE), p(iIRT).toDouble)
		}
		
		irtPeps
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