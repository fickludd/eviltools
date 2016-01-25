package se.lth.immun

import java.io.File
import scala.io.Source

import se.lth.immun.protocol.MSFragmentationProtocol.FragmentationType

object MSGF extends Interpret.IDs {

	import Interpret._
	
	val formatName = "MS-GF+ identification tsv" 
		
	case class MSGFid(
			val specID:SpectrumID,
			val specIndex:Int,
			val fragmentationType:FragmentationType,
			val precursorMz:Double,
			val isotopeError:Int,
			val precursorErrorPPM:Double,
			val z:Int,
			val pepSequence:String,
			val protein:String,
			val DeNovoScore:Int,
			val MSGFScore:Int,
			val specEValue:Double,
			val eValue:Double,
			val qValue:Double,
			val pepQValue:Double
	) extends ID {
		def score = - math.log10(eValue) 
		def psmLevelOk = qValue < params.psmFDR && eValue < params.eValue
	}
	
	val scanNumRE = """scan=(\d+)""".r.unanchored		

	def fromFile(f:File, params:InterpretParams):Seq[MSGFid] = {
		
		(for ( line <- Source.fromFile(f).getLines.drop(1) ) yield {
			val p = line.split("\t")
			
			val specIndex = p(2).toInt
			MSGFid(
				parseSpectrumID(specIndex, p(1)),
				specIndex,
				p(3) match {
					case "CID" => FragmentationType.CID
					case "HCD" => FragmentationType.HCD
					case "ETD" => FragmentationType.ETD
				},
				p(4).toDouble,
				p(5).toInt,
				p(6).toDouble,
				p(7).toInt,
				toUniModSequence(p(8)),
				cleanProteins(p(9)),
				p(10).toInt,
				p(11).toInt,
				p(12).toDouble,
				p(13).toDouble,
				p(14).toDouble,
				p(15).toDouble
			)
		}).toSeq
	}
	
	
	def toUniModSequence(pep:String):String = 
		pep.replaceAllLiterally("+15.995", "(UniMod:35)")
			.replaceAllLiterally("+57.021", "(UniMod:4)")
			
	def cleanProteins(proteins:String):String =
		proteins.split(";").map(_.takeWhile(_ != '(')).mkString(";")
}