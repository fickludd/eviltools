package se.lth.immun

import se.jt.Params
import java.io.File
import scala.io.Source

class TramlerParams extends Params {
	import Params._
	
	val in = ReqString("The input file. Can be csv or traml.")
	val ops = ReqString("operations to perform")
	
	val diaFile = ""	## "file with dia isolation windows. Can be used for trimming"
	
	val out = ""		## "file where output traml should be saved (default: input csv .traml)"
	val modFile = ""	## "file with modifications (one per line)"
	
	lazy val inFile = new File(in)
	def outFile =
		if (out.value == "") None
		else Some(new File(out))
		
	lazy val diaWindows = {
		for (line <- Source.fromFile(new File(diaFile)).getLines.toSeq) yield {
			val vals = line.split("\t", 2)
			Range.DRange(vals(0).toDouble, vals(1).toDouble)
		}
	}
}