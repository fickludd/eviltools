package se.lth.immun

import java.io.File
import scala.io.Source

import se.lth.immun.chem.Constants
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentationType

object PepCsv extends Interpret.IDs {

	import Interpret._
	val formatName = "TPP pep.xml derived pep.csv" 
	
	case class PepCsvId(
			val file:String,
			val scanNum:Int,
			val precursorMz:Double,
			val z:Int,
			val rtInSec:Double,
			val hitRank:Int,
			val pepSequence:String,
			val prevAA:String,
			val protein:String,
			val nMatchedIons:Int,
			val nTotalIons:Int,
			val numMissedCleavages:Int,
			val prob:Double,
			val qValue:Double
		) extends ID
	
	def fromFile(f:File, params:InterpretParams):Seq[PepCsvId] = {
		
		val mzMLBase = params.mzMLBaseName
		
		(for {
			line <- Source.fromFile(f).getLines.drop(1)
			if line.takeWhile(_ != '\t').contains(mzMLBase)
		} yield {
			val p = line.split("\t").map(_.filter(_ != '"'))
			
			val m = p(2).toDouble 
			val z = p(3).toInt
			
			PepCsvId(
				p(0),
				p(1).toInt,
				m / z + Constants.PROTON_WEIGHT,
				z,
				p(5).toDouble,
				p(6).toInt,
				toUniModSequence(p(7)),
				p(8),
				p(9),
				p(10).toInt,
				p(11).toInt,
				p(15).toInt,
				p(16).toDouble,
				p(17).toDouble
			)
		}).toSeq.filter(x => x.qValue < params.psmFDR && x.prob > params.peptideProb)
	}
	
	
	def toUniModSequence(pep:String):String =
		pep.replaceAllLiterally("[43.0184]", "(UniMod:1)")
			.replaceAllLiterally("[160.0306]", "(UniMod:4)")
			.replaceAllLiterally("E[111.0320]", "E(UniMod:27)")
			.replaceAllLiterally("Q[111.0321]", "Q(UniMod:28)")
			.replaceAllLiterally("[147.0354]", "(UniMod:35)")
}