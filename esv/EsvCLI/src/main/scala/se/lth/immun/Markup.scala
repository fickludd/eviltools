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

object Markup extends Command with CLIApplication {

	class F(
		val str:String,
		val lcol:String,
		val rcol:String,
		val f:(String, String) => Boolean
	) {}
	
	
	
	val str = "markup"
		
	var inLeft:File = null
	var left:EsvReader = null
	var inRight:File = null
	var right:EsvReader = null
	var outFile = new File("")
	var filters:Seq[F] = Nil
	var filterRows = 0
	
	def apply(args:Array[String]):Command = {
		
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
    		parseArgs("esv markup", args) 
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
		var nRows = 0
			
		while (!left.EOF) {
			out.write(left.values :+ (if (rowAny(left, fs)) "x" else " "))
			nRows += 1
			left.readLine
		}
		left.close
		out.close
		println(" num rows: "+nRows)
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

	
	def getOutEsv(orig:Esv):Esv = {
		orig.source = Some("esv markup")
		orig.addParameter("markuped by", inRight)
		var i = 1
		for (f <- filters) {
			orig.addParameter("markup filter "+i, f.str)
			i += 1
		}
		
		orig.headers = orig.headers :+ inRight.toString
			
		return orig
	}
	
	
	def parseFilter(s:String):F = {
		if (s.contains(" in ")) {
			var t = s.split(" in ", 2)
			return new F(s, t(0).trim,t(1).trim, (l:String, r:String) => {r.contains(l)})
		}
		
		throw new IOException("Couldn't parse '%s' as a filter".format(s))
	}
}