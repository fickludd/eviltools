package se.lth.immun

import se.jt.Params
import java.io.File
import scala.io.Source

class TramlerParams extends Params {
	import Params._
	
	val in = ReqString("The input file. Can be csv or traml.")
	val ops = ReqString("operations to perform")
	
	val diaFile = ""	## "tsv with dia isolation windows (no header, iso window low and high cols). Can be used for trimming"
	
	val outDir			= ""		## "directory where output traml should be saved"
	val outName			= ""		## "file where output traml should be saved (default: input csv .traml)"
	val modFile = ""	## "file with modifications (one per line)"
	
	lazy val inFile = new File(in)
	def outFile =
		if (outName.value == "") None
		else Some(new File(outDir, outName))
		
	lazy val diaWindows = {
		for (line <- Source.fromFile(new File(diaFile)).getLines.toSeq) yield {
			val vals = line.split("\t", 2)
			Range.DRange(vals(0).toDouble, vals(1).toDouble)
		}
	}
}