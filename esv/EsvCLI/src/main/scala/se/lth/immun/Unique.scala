package se.lth.immun

import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException
import java.io.File
import java.io.IOException
import java.io.FileReader
import java.io.FileWriter
import java.io.BufferedReader
import java.io.BufferedWriter
import se.lth.immun.esv._

import collection.mutable.HashSet

object Unique extends Command with CLIApplication {

		
	val str = "unique"
		
	var inFile:File = null
	var in:EsvReader = null
	var outFile = new File("")
	var cols:Seq[String] = null
	
	def apply(args:Array[String]):Command = {
		
		arg("ESV", s => {
    			inFile = new File(s)
    			in = new EsvReader(new BufferedReader(new FileReader(inFile)))
	    	})
		
		arg("OUT", s => {
    			outFile = new File(s)
	    	})
	    	
	    rest("COLS", rest => {
    			cols = rest
    		}, false)
    	
    	try {
    		parseArgs("esv unique", args) 
    	} catch {
    		case cae:CommandlineArgumentException => {
    			CLIApplication.log.write(cae)
    			System.exit(1)
    		}
    		case e:Exception => {
    			CLIApplication.log.write(e)
    			System.exit(2)
    		}
    	}
    	this
	}
	
	
	
	def execute = {
		val l = cols.length
		val found = new HashSet[Seq[String]]
		val out = new EsvWriter(getOutEsv(in), new BufferedWriter(new FileWriter(outFile)))
		var count = 0
		
		while (!in.EOF) {
			val a = new Array[String](l)
			for (i <- 0 until l)
				a(i) = in.getValue(cols(i))
			if (!found.contains(a)) {
				found += a
				out.write(in.values)
			}
			count += 1
			in.readLine
		}
		in.close
		out.close
		
		println("      num in rows: "+count)
		println("num filtered rows: "+found.size)
	}
	

	
	def getOutEsv(orig:Esv):Esv = {
		orig.addParameter("original file", inFile)
		var i = 1
		for (c <- cols) {
			orig.addParameter("unique col "+i, c)
			i += 1
		}
		return orig
	}
		
}