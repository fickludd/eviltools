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

object Filter extends Command with CLIApplication {

	class F(
		val str:String,
		val lcol:String,
		val rcol:String,
		val f:(String, String) => Boolean
	) {}
	
	
	
	val str = "filter"
		
	var inLeft:File = null
	var left:EsvReader = null
	var inRight:File = null
	var right:EsvReader = null
	var outFile = new File("")
	var filters:Seq[F] = Nil
	var filterRows = 0
	var rowOk:(EsvReader, Seq[(F, ArrayBuffer[String])]) => Boolean = rowAny
	var mode:String = "any"
	
	def apply(args:Array[String]):Command = {
		
		opt("any", "only keep rows that satisfy all filters on at least one filter row", 
			s => {
				rowOk = rowAny
				mode = "any"
			})
			
		opt("none", "only keep rows that don't satisfy all filters on any filter row", 
			s => {
				rowOk = rowNone
				mode = "none"
			})
		
		arg("LEFT", s => {
    			inLeft = new File(s)
    			left = new EsvReader(new BufferedReader(new FileReader(inLeft)))
	    	})
		
		arg("FILTER", s => {
    			inRight = new File(s)
    			right = new EsvReader(new BufferedReader(new FileReader(inRight)))
	    	})
		
		arg("OUT", s => {
    			outFile = new File(s)
	    	})
	    	
	    rest("FILTERS", rest => {
    			filters = rest.map(parseFilter _)
    		}, false)

    	try {
    		parseArgs("esv filter", args) 
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
		var fs = filters.map(f => (f, new ArrayBuffer[String]))
		while (!right.EOF) {
			for (t <- fs)
				t._2 += right.getValue(t._1.rcol)
			right.readLine
		}
		right.close
		filterRows = fs.head._2.length
		
		println("num filter rows: "+filterRows)
		val out = new EsvWriter(getOutEsv(left), new BufferedWriter(new FileWriter(outFile)))
		var inRows = 0
		var outRows = 0
			
		while (!left.EOF) {
			if (rowOk(left, fs)) {
				out.write(left.values)
				outRows += 1
			}
			
			inRows += 1
			left.readLine
		}
		left.close
		out.close
		println(" num in rows: "+inRows)
		println("num out rows: "+outRows)
	}



	def rowAny(row:EsvReader, fs:Seq[(F, ArrayBuffer[String])]):Boolean = {
		for (i <- 0 until filterRows)
			if (fs.forall(t => {
						val f = t._1; 
						f.f(left.getValue(f.lcol), t._2(i))
					}))
				return true
		
		return false
	}



	def rowNone(row:EsvReader, fs:Seq[(F, ArrayBuffer[String])]):Boolean = {
		for (i <- 0 until filterRows)
			if (fs.forall(t => {
						val f = t._1; 
						f.f(left.getValue(f.lcol), t._2(i))
					}))
				return false
		
		return true
	}
	

	
	def getOutEsv(orig:Esv):Esv = {
		orig.source = Some("esv filter")
		orig.addParameter("filter mode", mode)
		orig.addParameter("filtered by", inRight)
		var i = 1
		for (f <- filters) {
			orig.addParameter("filter "+i, f.str)
			i += 1
		}
			
		return orig
	}
	
	
	def parseFilter(s:String):F = {
		if (s.contains(" in ")) {
			var t = s.split(" in ", 2)
			return new F(s, t(0).trim, t(1).trim, (l:String, r:String) => {r.contains(l)})
		}
		if (s.contains(" == ")) {
			var t = s.split(" == ", 2)
			return new F(s, t(0).trim, t(1).trim, _ == _)
		}
		
		throw new IOException("Couldn't parse '%s' as a filter".format(s))
	}
}