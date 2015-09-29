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

object Clean extends Command with CLIApplication {

	class F(
		val str:String,
		val lcol:String,
		val f:(String) => Boolean
	) {}
	
	
	
	val str = "clean"
		
	var inLeft:File = null
	var left:EsvReader = null
	var outFile = new File("")
	var filters:Seq[F] = Nil
	
	def apply(args:Array[String]):Command = {
		
		arg("ESV", s => {
    			inLeft = new File(s)
    			left = new EsvReader(new BufferedReader(new FileReader(inLeft)))
	    	})
		
		arg("OUT", s => {
    			outFile = new File(s)
	    	})
	    	
	    rest("FILTERS", rest => {
    			filters = rest.map(parseFilter _)
    		}, false)

    	try {
    		parseArgs("esv clean", args) 
    	} catch {
    		case cae:CommandlineArgumentException => {
    			CLIApplication.log.write(cae)
    			println(cae)
    			System.exit(1)
    		}
    		case e:Exception => {
    			CLIApplication.log.write(e)
    			println(e)
    			System.exit(2)
    		}
    	}
		this
	}
	
	
	
	def execute = {
		val out = new EsvWriter(getOutEsv(left), new BufferedWriter(new FileWriter(outFile)))
		var inRows = 0
		var outRows = 0
		
		println(outFile)
		println(inLeft)
		for (f <- filters)
			println(f.str)
		
		while (!left.EOF) {
			if (rowOk(left)) {
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



	def rowOk(row:EsvReader):Boolean = {
		if (filters.forall(f => f.f(row.getValue(f.lcol)) ))
			return true
		
		return false
	}


	
	def getOutEsv(orig:Esv):Esv = {
		orig.source = Some("esv clean")
		var i = 1
		for (f <- filters) {
			orig.addParameter("clean filter "+i, f.str)
			i += 1
		}
			
		return orig
	}
	
	
	def parseFilter(s:String):F = {
		var col = ""
		var lit = ""
		var litD = Double.NaN
		var f:F = null
		def isFilter(x:String, func:String => Boolean):Boolean = {
			if (s.contains(x)) {
				var t = s.split(x, 2)
				col = t(0).trim
				lit = t(1).trim
				try {
					litD = lit.toDouble
				} catch {
					case _:Throwable => litD = Double.NaN
				}
				f = new F(s, col, func)
				return true
			}
			return false
		}
		
		
		if (isFilter(" in ", (l:String) => lit.contains(l) ))
			return f
		
		if (isFilter(" !in ", (l:String) => !lit.contains(l) ))
			return f
		
		if (isFilter(" == ", (l:String) => lit == l ))
			return f
		
		if (isFilter(" != ", (l:String) => lit != l ))
			return f
			
			
			
		if (isFilter(" d== ", (l:String) => litD == l.toDouble ))
			return f
		
		if (isFilter(" d!= ", (l:String) => litD != l.toDouble ))
			return f
		
		if (isFilter(" <= ", (l:String) => l.toDouble <= litD))
			return f
		
		if (isFilter(" >= ", (l:String) => l.toDouble >= litD))
			return f
		
		if (isFilter(" < ", (l:String) => l.toDouble < litD))
			return f
		
		if (isFilter(" > ", (l:String) => l.toDouble > litD ))
			return f
		
		throw new IOException("Couldn't parse '%s' as a filter".format(s))
	}
}