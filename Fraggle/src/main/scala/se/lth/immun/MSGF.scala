package se.lth.immun

import java.io.File
import scala.io.Source

import se.lth.immun.protocol.MSFragmentationProtocol.FragmentationType

object MSGF {

	case class MSGFid(
			val specId:String,
			val scanNum:Int,
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
			val pepQValue:Double)
	
	val scanNumRE = """scan=(\d+)""".r.unanchored		

	def fromFile(f:File, fdrCutoff:Double):Seq[MSGFid] = {
		
		(for ( line <- Source.fromFile(f).getLines.drop(1) ) yield {
			val p = line.split("\t")
			
			val scanNum = 
				p(1) match {
					case scanNumRE(x) => x.toInt
					case _ => -1
				}
			
			MSGFid(
				p(1),
				scanNum,
				p(2).toInt,
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
				p(9),
				p(10).toInt,
				p(11).toInt,
				p(12).toDouble,
				p(13).toDouble,
				p(14).toDouble,
				p(15).toDouble
			)
		}).toSeq.filter(_.qValue < fdrCutoff)
	}
	
	
	def toUniModSequence(pep:String):String = 
		pep.replaceAllLiterally("+15.995", "(UniMod:35)")
			.replaceAllLiterally("+57.021", "(UniMod:4)")
}