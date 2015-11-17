package se.lth.immun

import java.io.File
import scala.io.Source

import se.lth.immun.xl.masterformat.XLMasterReader

object Kojak extends Interpret.IDs {

	import Interpret._
	
	val formatName = "Kojak+percolator based XL master tsv"
	
	case class KojakId(
			val psmID:String,
			val label:String,
			val specID:SpectrumID,
			val score:Double,
			val z:Int,
			val precursorMz:Double,
			val pepSequence:String,
			val protein:String,
			val qValue:Double,
			val postErrProb:Double
	) extends ID {
		def psmLevelOk =
			qValue < params.psmFDR
	}
	
	def fromFile(f:File, params:InterpretParams):Seq[KojakId] = {
		
		val masterRows = XLMasterReader.read(f.getPath)
		for (r <- masterRows) yield {
			val idParts = r.kojak.psmID.split("-")
			KojakId(
					r.kojak.psmID,
					idParts(0),
					ScanID(idParts(1).toInt, idParts(1)),
					r.kojak.score,
					r.common.z,
					r.common.mz,
					r.common.xlink,
					r.kojak.proteinIDs.mkString(";"),
					r.kojak.qValue,
					r.kojak.posteriorErrorProb
				)
		}
	}
}