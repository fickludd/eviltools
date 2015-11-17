package se.lth.immun

import collection.mutable.Queue
import collection.mutable.ArrayBuffer
import collection.mutable.HashSet

import java.io.BufferedReader
import se.lth.immun.files.Delimited
import se.lth.immun.traml.clear._
import se.lth.immun.chem.Constants

object CsvParser {
	
	val Q1 = Array("q1", "precursormz")
	val Q3 = Array("q3", "productmz")
	val CE = Array("ce", "collisionenergy")
	val SEQ = Array("sequence", "peptidesequence", "seq", "peptide")
	val PROT = Array("proteinname", "protein", "accession", "proteinid")
	val FRAG = Array("fragment", "fragmention")
	val Q1Z = Array("q1z", "precursorcharge")
	val Q3Z = Array("q3z", "productcharge")
	val RT = Array("rt", "retentiontime")
	val iRT = Array("irt", "ntime")
	val IC = Array("intensity", "area", "libraryintensity", "rel_area")
	val SCORE = Array("score", "ddbscore")
	val ACC = Array("acc", "accession", "proteinaccession")
	val LABEL = Array("label", "isotopeLabel")
	val LABELGROUP = Array("labelgroup")
	
	
	val USAGE = "understood columns (not case-sensistive):"+
				"\n    Q1         " + Q1.mkString(", ")+
				"\n    Q3         " + Q3.mkString(", ")+
				"\n    CE         " + CE.mkString(", ")+
				"\n    SEQ        " + SEQ.mkString(", ")+
				"\n    PROT       " + PROT.mkString(", ")+
				"\n    FRAG       " + FRAG.mkString(", ")+
				"\n    Q1Z        " + Q1Z.mkString(", ")+
				"\n    Q3Z        " + Q3Z.mkString(", ")+
				"\n    RT (sec)   " + RT.mkString(", ")+
				"\n    iRT        " + iRT.mkString(", ")+
				"\n    IC         " + IC.mkString(", ")+
				"\n    SCORE      " + SCORE.mkString(", ")+
				"\n    ACC        " + ACC.mkString(", ")+
				"\n    LABEL      " + LABEL.mkString(", ")+
				"\n    LABELGROUP " + LABELGROUP.mkString(", ") +
				"\n\n\n the different base-type of csvs that are parseable are"+
				"\n"+
				"\n    type              minimum columns"+
				"\n   ----------------  ------------------------------"+
				"\n   transition list   Q1, Q3, CE, SEQ, PROT and FRAG"+
				"\n   peptide list      SEQ, PROT"+
				"\n   compound list     Q1, Q1z"
				
	
	
	class Chars(
		val sep:Char,
		val quote:Char
	) {}
	
	class Cols(
		val q1:Int,
		val q3:Int,
		val ce:Int,
		val seq:Int,
		val prot:Int,
		val frag:Int,
		val q1z:Int 		= -1,
		val q3z:Int 		= -1,
		val rt:Int	 		= -1,
		val irt:Int	 		= -1,
		val intensity:Int	= -1,
		val score:Int		= -1,
		val acc:Int			= -1,
		val label:Int		= -1,
		val labelGroup:Int		= -1
	) {
	
		
		def transitionCsv =
			q1 >= 0 && q3 >= 0 && ce >= 0 &&
			seq >= 0 && prot >= 0 && frag >= 0
			
		def peptideCsv =
			seq >= 0 && prot >= 0
			
		def compoundCsv =
			q1 >= 0 && q1z >= 0
		/*
		require(q1 >= 0, "q1 column should be >= 0")
		require(q3 >= 0, "q3 column should be >= 0")
		require(ce >= 0, "ce column should be >= 0")
		require(seq >= 0, "seq column should be >= 0")
		require(prot >= 0, "prot column should be >= 0")
		require(frag >= 0, "frag column should be >= 0")
		
		*/
	}
	
	
	def isNum(str:String) = 
		try {
			str.toDouble
			true
		} catch {
			case _:Throwable => false
		}
	
	def isFrag(str:String) = 
		try {
			str.tail.toInt
			Array('a', 'b', 'c', 'x', 'y', 'z').contains(str.head)
		} catch {
			case _:Throwable => false
		}
	
	
		
		
	def fromFile(inCsv:BufferedReader):ClearTraML = {
		
		val nextLines = new Queue[String]
		def readAhead(n:Int):Unit = {
			for (i <- 0 until n) {
				val line = inCsv.readLine
				if (line == null)
					return 
				nextLines += line
			}
		}
		def nextLine:Option[String] = {
			val line = 
					if (nextLines.nonEmpty) nextLines.dequeue
					else 					inCsv.readLine
			if (line == null) None
			else Some(line)
		}
		readAhead(10)
		
		
		
		val rr:String => List[String] = {
			val commaCount 	= nextLines.map(_.count(_ == ',')).sum
			val tabCount 	= nextLines.map(_.count(_ == '\t')).sum
			(row:String) => Delimited.readRow(if (commaCount > tabCount) ',' else '\t', '"', row)
		}
		
		val rows = nextLines.map(rr)
		val hasHeader = !rows.head.exists(isNum)
		val protRE		= "(.*[a-z].*[a-z].*)|(.*[A-Z].*[0-9].*)".r
		
		
		val cols = {
			if (hasHeader) {
				val header = rows.head
				def indexOf(l:Seq[String]) = 
					header.indexWhere(x => l.contains(x.toLowerCase))
				
				new Cols(
						indexOf(Q1),
						indexOf(Q3),
						indexOf(CE),
						indexOf(SEQ),
						indexOf(PROT),
						indexOf(FRAG),
						indexOf(Q1Z),
						indexOf(Q3Z),
						indexOf(RT),
						indexOf(iRT),
						indexOf(IC),
						indexOf(SCORE),
						indexOf(ACC),
						indexOf(LABEL),
						indexOf(LABELGROUP)
					)
			} else {
				val first 		= rows.head
				val cols 		= rows.transpose
				val numCols 	= cols.filter(_.forall(isNum))
				cols.indexWhere(_.forall(x => x.contains('a' to 'z') ))
				new Cols(
						first.indexOf(numCols(0)(0)),
						first.indexOf(numCols(1)(0)),
						first.indexOf(numCols(2)(0)),
						first.indexWhere(_.forall("GASPVTILNDKQEMHFRYWC".toCharArray.contains)),
						cols.indexWhere(_.forall(x => protRE.findFirstIn(x).nonEmpty)),
						cols.indexWhere(_.forall(isFrag))
					)
			}
		}
		
		
		
    	if (hasHeader) nextLine
    	
    	
    	
    	if (cols.transitionCsv)
    		parseTransitionCsv(() => nextLine.map(rr), cols)
    	else if (cols.peptideCsv)
    		parsePeptideCsv(() => nextLine.map(rr), cols)
    	else if (cols.compoundCsv)
    		throw new Exception("Reading compound csv's in currently not implemented!")
    		//parseCompoundCsv(() => nextLine.map(rr), cols)
    	else
    		throw new IllegalArgumentException("Could not parse csv!")
	}
	
	
	
	
	def parseTransitionCsv(
			nextLine:() => Option[List[String]], 
			cols:Cols
	):ClearTraML = {
    	
    	val b 	= new ClearTraMLBuilder
    	
    	var line = nextLine()
    	while (line.isDefined) {
    		val vals = line.get
    		val prot = b.addProtein(vals(cols.prot))
    		
    		val cp = b.addPeptide(vals(cols.seq), Array(prot))
    		
    		import PeptideParser._
    		lazy val pepMass:Option[Double] =
    			PeptideParser.parseSequence(cp.id) match {
	    			case UniModPeptide(p) => Some(p.monoisotopicMass)
	    			case XLinkPeptide(xl) => Some(xl.monoisotopicMass)
	    			case _ => None
	    		}
    		
    		val q1 = vals(cols.q1).toDouble
    		val q1z =
    			if (cols.q1z >= 0)
	    			vals(cols.q1z).toInt
	    		else if (pepMass.isDefined)
	    			math.round(pepMass.get / q1).toInt
	    		else 
	    			throw new Exception("Unable to parse or compute precursor charge for line: "+line)
	    			    	
		    val q3 = vals(cols.q3).toDouble
		    val channel = 
		    	Clear.Channel(
		    		q3,
		    		vals(cols.q3z).toInt,
		    		if (cols.frag >= 0) vals(cols.frag)
		    		else "%s @ %.3f / %.3f".format(cp.id, q1, q3),
		    		2,
		    		if (cols.intensity >= 0) Some(vals(cols.intensity).toDouble) else None
		    	)
    		
		    val a = cp.getAssay(q1, q1z, None)
		    a.ms2Channels += channel
		    	
    		line = nextLine()
    	}
    	
		return b.result
	}
	
	
	
	
	def parsePeptideCsv(
			nextLine:() => Option[List[String]], 
			cols:Cols
	):ClearTraML = {
		val b = new ClearTraMLBuilder
		
		var line = nextLine()
    	while (line.isDefined) {
    		val vals = line.get
    		val prot = b.addProtein(vals(cols.prot))
    		
    		val cp = b.addPeptide(vals(cols.seq), Array(prot))
    			
    		line = nextLine()
    	}
		
		return b.result
	}
	
	
	
	
	/*def parseCompoundCsv(
			nextLine:() => Option[List[String]], 
			cols:Cols
	):GhostTraML = {
		val outTraml 	= new GhostTraML
		
		var line = nextLine()
    	while (line.isDefined) {
    		val vals = line.get
    		val q1 = vals(cols.q1).toDouble
    		val q1z = vals(cols.q1z).toInt
    		val m = q1*q1z - q1z*Constants.PROTON_WEIGHT
    		val id = "Unknown molecule: %.5f Da".format(m)
    		if (!outTraml.compounds.contains(id)) {
    			val gComp = new GhostCompound
    			gComp.id = id
    			gComp.mass = m
    			gComp.preferredCharges = gComp.preferredCharges :+ q1z
    			if (cols.label >= 0)
    				gComp.label = vals(cols.label)
    			if (cols.labelGroup >= 0)
    				gComp.labelGroup = vals(cols.labelGroup)
    			outTraml.compounds += gComp.id -> gComp
    		}
    		
    		line = nextLine()
    	}
		
		return outTraml
	}*/
}