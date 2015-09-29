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

object Concat extends Command with CLIApplication {

	class F(
		val str:String,
		val lcol:String,
		val rcol:String,
		val f:(String, String) => Boolean
	) {}
	
	val str = "concat"
		
	var files:Seq[File] = Nil
	var outFile = new File("concat.esv")
	
	def apply(args:Array[String]):Command = {
			    	
		opt("out", "file to save output (default: concat.esv)", 
			s => {
				outFile = new File(s)
			}, "X")
		
	    rest("FILES", rest => {
	    		files 	= rest.map(s => new File(s))
    		}, false)
    		
    	try {
    		parseArgs("esv concat", args) 
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
		val first = new EsvReader(new BufferedReader(new FileReader(files.head)))
		val out = new EsvWriter(getOutEsv(first), new BufferedWriter(new FileWriter(outFile)))
		var nOut = 0
		
		while (!first.EOF) {
			out.write(first.values)
			nOut += 1
			first.readLine
		}
		first.close

		for (f <- files.tail) {
			val esv = new EsvReader(new BufferedReader(new FileReader(f)))
			while (!esv.EOF) {
				var a = new ArrayBuffer[String]
				for (h <- first.headers)
					try {
						a += esv.getValue(h)
					} catch {
						case _:Throwable => a += ""
					}
				out.write(a)
				nOut += 1
				esv.readLine
			}
			esv.close
		} 
		out.close
		println("num out rows: "+nOut)
	}

	
	
	
	
	def getOutEsv(orig:Esv):Esv = {
		orig.source = Some("esv concat")
		var i = 1
		for (f <- files) {
			orig.addParameter("concated file "+i, f)
			i += 1
		}
			
		return orig
	}
}