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

object Partition extends Command with CLIApplication {
	
	val str = "partition"
		
	var partCols:Seq[String] = Nil
	var files:Seq[File] = Nil
	var inFile:File = null
	var inFileName:String = ""
	var inEsv:EsvReader = null
	var outDir = new File("./")
	var shuffle = false
	var ordered = false
	var partitionSize = 0
	
	def apply(args:Array[String]):Command = {
			    	
		opt("shuffle", "shuffle rows randomly into partitions", 
			s => {
				shuffle = true
			})
			    	
		opt("ordered", "if already ordered by partition columns this speeds things up", 
			s => {
				ordered = true
			})
		
		opt("on", "comma-separated list of columns to partition on", 
			s => {
				partCols = s.split(",").map(_.trim)
			}, "X")
		
		opt("out", "dir to store outputs (default: ./)", 
			s => {
				outDir = new File(s)
			}, "X")
		
	    arg("FILE", s => {
	    		inFile 	= new File(s)
	    		if (s.toLowerCase().endsWith(".esv"))
	    			inFileName = s.split("/").last.dropRight(4)
	    		else
	    			inFileName = s.split("/").last
	    		inEsv = new EsvReader(new BufferedReader(new FileReader(inFile)))
    		})
		
	    arg("PARTITION_SIZE", s => {
	    		partitionSize = s.toInt
    		})
    		
    	try {
    		parseArgs("esv partition", args) 
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
		
		var count = 0
		var partNo = 0
		var rowCount = 0
		var out:EsvWriter = null
		
		def getOut = {
			var outFile = new File(outDir,"%s_%03d.esv".format(inFileName, partNo))
			new EsvWriter(getOutEsv(inEsv, partNo), new BufferedWriter(new FileWriter(outFile)))
		}
		out = getOut
		
		def nextOut = {
			out.close
			partNo += 1
			out = getOut
			count = 0
		}
			
		
		if (ordered && !shuffle) {
			var lastKey:Seq[String] = null
			
			while (!inEsv.EOF) {
				val key = partCols.map(c => inEsv.getValue(c))
				if (key != lastKey) {
					count += 1
					if (count > partitionSize) 
						nextOut
					lastKey = key
				}
				rowCount += 1
				inEsv.readLine
			}
			
			out.close
		} else if (shuffle) {
			val dict = new HashMap[Seq[String], ArrayBuffer[Seq[String]]]
			
			while (!inEsv.EOF) {
				val key = partCols.map(c => inEsv.getValue(c))
				if (!dict.contains(key))
					dict += key -> new ArrayBuffer[Seq[String]]
				dict(key) += inEsv.values
				rowCount += 1
				inEsv.readLine
			}
			
			println("num partition keys: "+dict.size)
			
			for ((key, rows) <- dict) {
				count += 1
				if (count > partitionSize)
					nextOut
				
				for (row <- rows)
					out.write(row)
			}
		} else {
			val list = new ArrayBuffer[(Seq[String], ArrayBuffer[Seq[String]])]
			
			while (!inEsv.EOF) {
				val key = partCols.map(c => inEsv.getValue(c))
				list.find(_._1 == key) match {
					case Some(t) => t._2 += inEsv.values
					case None => {
						list += key -> ArrayBuffer[Seq[String]](inEsv.values)
					}
				}
				rowCount += 1
				inEsv.readLine
			}
			
			for ((key, rows) <- list) {
				count += 1
				if (count > partitionSize)
					nextOut
				
				for (row <- rows)
					out.write(row)
			}
		}
		
		out.close
		println("num out rows: "+rowCount)
	}

	
	
	
	
	def getOutEsv(orig:Esv, partNo:Int):Esv = {
		orig.source = Some("esv partition")
		orig.addParameter("partition num", partNo)
		orig.addParameter("partition cols", partCols.mkString(" "))
		return orig
	}
}