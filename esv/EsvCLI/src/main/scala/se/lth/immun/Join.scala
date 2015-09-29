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

object Join extends Command with CLIApplication {

	class F(
		val str:String,
		val lcol:String,
		val rcol:String,
		val f:(String, String) => Boolean
	) {}
	
	val str = "join"
		
	var inLeft:File = null
	var left:EsvReader = null
	var inRight:File = null
	var right:EsvReader = null
	var outFile = new File("")
	var filters:Seq[F] = Nil
	var cols:Seq[String] = Nil
	var filterRows = 0
	var mode:String = "left"
	
	def apply(args:Array[String]):Command = {
		
		opt("mode", "mode for join, inner|left (default: left)", 
			s => {
				mode = s
			}, "X")
			
		arg("LEFT", s => {
    			inLeft = new File(s)
    			left = new EsvReader(new BufferedReader(new FileReader(inLeft)))
	    	})
		
		arg("RIGHT", s => {
    			inRight = new File(s)
    			right = new EsvReader(new BufferedReader(new FileReader(inRight)))
	    	})
		
		arg("OUT", s => {
    			outFile = new File(s)
	    	})
	    	
	    rest("FILTERS", rest => {
	    		val fs 	= rest.map(parseFilter _)
	    		filters = fs.filter(_.lcol != null)
	    		cols 	= fs.filter(_.lcol == null).map(_.rcol)
    		}, false)
    		
    	try {
    		parseArgs("esv join", args) 
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
		var cs = if (cols.isEmpty)
				right.headers.map(h => (h, new ArrayBuffer[String]))
			else
				cols.map(c => (c, new ArrayBuffer[String]))
		
		while (!right.EOF) {
			for (t <- fs)
				t._2 += right.getValue(t._1.rcol)
			for (t <- cs)
				t._2 += right.getValue(t._1)
			right.readLine
		}
		right.close
		filterRows = fs.head._2.length
		println("num filter rows: "+filterRows)
		
		
		val out = new EsvWriter(getOutEsv(left, cs.map(_._1)), new BufferedWriter(new FileWriter(outFile)))
		var inRows = 0
		var outRows = 0
			
		while (!left.EOF) {
			val joined = rows(left, fs, cs)
			for (r <- joined) {
				out.write(r)
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



	def rows(
			row:EsvReader, 
			fs:Seq[(F, ArrayBuffer[String])], 
			cols:Seq[(String, Seq[String])]
	):Seq[Seq[String]] = {
		val r = new ArrayBuffer[Seq[String]]
		for (i <- 0 until filterRows)
			if (fs.forall(t => {
						val f = t._1; 
						f.f(left.getValue(f.lcol), t._2(i))
					}))
				r += row.values ++ cols.map(_._2(i))
		
		if (r.isEmpty && mode == "left")
			r += row.values ++ cols.map(_ => "")
		return r
	}
	

	
	def getOutEsv(orig:Esv, extraHeaders:Seq[String]):Esv = {
		orig.source = Some("esv join")
		orig.headers = orig.headers ++ extraHeaders
		orig.addParameter("join mode", mode)
		orig.addParameter("joined left", inLeft)
		orig.addParameter("joined right", inRight)
		var i = 1
		for (f <- filters.filter(_.lcol != null)) {
			orig.addParameter("join col "+i, f.str)
			i += 1
		}
		for (f <- filters.filter(_.lcol == null)) {
			orig.addParameter("add right col "+i, f.rcol)
			i += 1
		}
			
		return orig
	}
	
	
	
	def parseFilter(s:String):F = {
		if (s.contains(" in ")) {
			var t = s.split(" in ", 2)
			return new F(s, t(0).trim, t(1).trim, (l:String, r:String) => {r.contains(l)})
		} else if (s.contains(" == ")) {
			var t = s.split(" == ", 2)
			return new F(s, t(0).trim, t(1).trim, _ == _)
		} else 
			return new F(s, null, s.trim, _ == _)
		
		throw new IOException("Couldn't parse '%s' as a filter".format(s))
	}
}