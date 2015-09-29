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
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

object Count extends Command with CLIApplication {
	
	val str = "count"
		
	var countCols:Seq[String] = Nil
	var inFile:File = null
	var in:EsvReader = null
	var outFile = new File("")
	
	def apply(args:Array[String]):Command = {
			    	
		arg("IN", s => {
    			inFile = new File(s)
    			in = new EsvReader(new BufferedReader(new FileReader(inFile)))
	    	})
		
		arg("OUT", s => {
    			outFile = new File(s)
	    	})
	    	
	    rest("COLUMNS", rest => {
    			countCols = rest.map(_.trim)
    		}, false)
    		
    	try {
    		parseArgs("esv count", args) 
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
		
		var inRows = 0
		val dict = new HashMap[Seq[String], Int]
		var lastKey:Seq[String] = null
			
		while (!in.EOF) {
			val key = countCols.map(c => in.getValue(c))
			if (!dict.contains(key))
				dict += key -> 1
			else
				dict(key) += 1
			
			inRows += 1
			in.readLine
		}
			
		var out = new EsvWriter(getOutEsv(in), new BufferedWriter(new FileWriter(outFile)))
		for ((key, count) <- dict) {
			out.write(key :+ count)
		}
		out.close
		
		println("num in rows: "+inRows)
		println("num out rows: "+dict.size)
	}

	
	
	
	
	def getOutEsv(orig:Esv):Esv = {
		orig.source = Some("esv count")
		orig.addParameter("count cols", countCols.mkString(" "))
		orig.headers = countCols :+ "count"
		orig.nColumns = Some(countCols.length + 1)
		return orig
	}
}