package se.lth.immun

import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

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
		
		var iFILE = -1
		var iSCAN = -1
		var iPREC_MASS = -1
		var iZ = -1
		var iINDEX = -1
		var iRT_IN_SEC = -1
		var iHIT_RANK = -1
		var iPEPTIDE = -1
		var iPEPTIDE_PREV_AA = -1
		var iPROTEIN = -1
		var iNUM_TOT_PROTEINS = -1
		var iNUM_MATCHED_IONS = -1
		var iTOT_NUM_IONS = -1
		var iCALC_NEUTRAL_PEP_MASS = -1
		var iMASS_DIFF = -1
		var iNUM_TOL_TERM = -1
		var iNUM_MISSED_CLEAVAGES = -1
		var iPROB = -1
		var iQ_VALUE = -1

		var headerParsed = false
		
		val pepIDs = new ArrayBuffer[PepCsvId]
		for (line <- Source.fromFile(f).getLines) {
			val p = line.split("\t").map(_.filter(_ != '"'))
			
			if (!headerParsed) {
				iFILE = p.indexOf("file")
				iSCAN = p.indexOf("scan")
				iPREC_MASS = p.indexOf("precMass")
				iZ = p.indexOf("z")
				iINDEX = p.indexOf("index")
				iRT_IN_SEC = p.indexOf("rtInSec")
				iHIT_RANK = p.indexOf("hitRank")
				iPEPTIDE = p.indexOf("peptide")
				iPEPTIDE_PREV_AA = p.indexOf("peptidePrevAA")
				iPROTEIN = p.indexOf("protein")
				iNUM_TOT_PROTEINS = p.indexOf("numTotProteins")
				iNUM_MATCHED_IONS = p.indexOf("numMatchedIons")
				iTOT_NUM_IONS = p.indexOf("totNumIons")
				iCALC_NEUTRAL_PEP_MASS = p.indexOf("calcNeutralPepMass")
				iMASS_DIFF = p.indexOf("massDiff")
				iNUM_TOL_TERM = p.indexOf("numTolTerm")
				iNUM_MISSED_CLEAVAGES = p.indexOf("numMissedCleavages")
				iPROB = p.indexOf("prob")
				iQ_VALUE = p.indexOf("q-value")
				headerParsed = true
			} else {
				val m = p(iPREC_MASS).toDouble 
				val z = p(iZ).toInt
				val f = p(iFILE)
				
				if (f == mzMLBase)
					pepIDs += PepCsvId(
						f,
						p(iSCAN).toInt,
						m / z + Constants.PROTON_WEIGHT,
						z,
						p(iRT_IN_SEC).toDouble,
						p(iHIT_RANK).toInt,
						toUniModSequence(p(iPEPTIDE)),
						p(iPEPTIDE_PREV_AA),
						p(iPROTEIN),
						p(iNUM_MATCHED_IONS).toInt,
						p(iTOT_NUM_IONS).toInt,
						p(iNUM_MISSED_CLEAVAGES).toInt,
						p(iPROB).toDouble,
						p(iQ_VALUE).toDouble
					)
			}
		}
			
			
		pepIDs.filter(x => 
				x.qValue < params.psmFDR && 
				x.prob > params.peptideProb
			)
	}
	
	
	def toUniModSequence(pep:String):String =
		pep.replaceAllLiterally("[43.0184]", "(UniMod:1)")
			.replaceAllLiterally("C[160.0306]", "C(UniMod:4)")
			.replaceAllLiterally("C[143.0041]", "C(UniMod:26)")
			.replaceAllLiterally("E[111.0320]", "E(UniMod:27)")
			.replaceAllLiterally("Q[111.0321]", "Q(UniMod:28)")
			.replaceAllLiterally("M[147.0354]", "M(UniMod:35)")
}