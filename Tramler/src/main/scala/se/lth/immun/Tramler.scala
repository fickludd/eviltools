package se.lth.immun

import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException

import se.lth.immun.xml.XmlWriter

import java.io.File
import java.io.FileReader
import java.io.Reader
import java.io.BufferedReader
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Properties
import collection.mutable.ArrayBuffer
import collection.mutable.HashSet
import collection.JavaConversions._

import se.lth.immun.traml.ghost._
import se.lth.immun.xml._
import se.lth.immun.chem._

object Tramler extends CLIApplication {
	
	trait ReadMode
	case class InCsv() extends ReadMode
	case class InTraml() extends ReadMode
	case class Unknown() extends ReadMode
	
	var inFile:File 		= _
	var readMode:ReadMode 	= _
	var outTramlFile:Option[File] = None
	var outputDefault:File 	= _
	var modFile:File		= _
	var mods:Seq[IModifier] 	= Nil
	var traml:GhostTraML 	= _
	var ops:Seq[TramlOperation] = Nil
	
	def isExt(f:File, ext:String) =
		f.toString.toLowerCase.endsWith(ext)
	
	def replaceExt(f:File, oldExt:String, ext:String) =
		new File(f.toString.dropRight(oldExt.length) + ext)
	
	def appendExt(f:File, ext:String) =
		new File(f.toString + ext)
	
	
	def parseOp(str:String):TramlOperation = {
		def getOp(opString:String) = opString match {
			case Decoy.opString => new Decoy()
			case Isotopes.opString => new Isotopes()
			case Include.opString => new Include()
			case Clean.opString => new Clean()
			case Subsample.opString => new Subsample()
			case Stats.opString => new Stats()
			case _ =>
				throw new IllegalArgumentException("'"+str+"' is not a valid traml operation!")
		}
		
		if (str.contains("(")) {
			val parts 	= str.split("\\(",2)
			val op 		= getOp(parts(0))
			val params 	= parts(1).init
			op.params = params
			try {
				op.setup(params, mods)
			} catch {
				case e:Exception =>
					throw new IllegalArgumentException(
							"\n   == "+parts(0)+" ==\n"+
							e.getMessage() + "\n" + op.usage
						, e)
			}
			op
		} else 
			getOp(str)
	}
	
	def main(args:Array[String]):Unit = {
		
		var properties = new Properties
    	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
    	    	
		arg("IN_FILE", s => {
			inFile = new File(s)
			outputDefault =
				if (isExt(inFile, ".traml")) {
					readMode = InTraml()
					appendExt(inFile, ".out")
				} else if (isExt(inFile, ".csv") || 
							isExt(inFile, ".txt") || 
							isExt(inFile, ".tsv")
				) {
					readMode = InCsv()
					replaceExt(inFile, ".csv", ".traml")
				} else {
					readMode = Unknown()
					appendExt(inFile, ".traml")
				}
		})
		
		rest("OPS", opStrings => {
			ops = opStrings.map(s => {
				val op = parseOp(s)
				op match {
					case d:Decoy =>
						outputDefault = 
							if (isExt(outputDefault, ".out")) 
								replaceExt(outputDefault, ".traml.out", ".decoy.traml")
							else
								replaceExt(outputDefault, ".traml", ".decoy.traml")
					case _ => {}
				}
				op
			})
			
		}, true)
		
		opt("output", 
				"file where output traml should be saved (default: input csv .traml)", 
				s => {
					outTramlFile = Some(new File(s))
				},
				"X")
		
		opt("mods", 
				"file with modifications (one per line)", 
				s => {
					modFile = new File(s)
					mods = readModFile(modFile)
				},
				"X")
		
		
		val before 		= System.currentTimeMillis
    	val name 		= properties.getProperty("pom.name")
    	val version 	= properties.getProperty("pom.version")
    	
		try {
			parseArgs(name + " "+version, args)
		} catch {
			case cae:CommandlineArgumentException => 
				println(cae.toString)
				println
				println(CsvParser.USAGE)
				println
				println(operationsHelp)
				println
			
				return
		}
		
		println(name + " "+version)
		println("       input file: "+inFile)
    	
		traml = readInFile
    	
		for (op <- ops) {
			println("  [%s]".format(op.toString))
			traml = op.operate(traml)
		}
    	
		val outFile = outTramlFile.getOrElse(outputDefault)
		println("    [OUTPUT] file: "+outFile)
    	println()
    	
    	traml.write(new XmlWriter(new BufferedWriter(new FileWriter(outFile))))
		
		val after = System.currentTimeMillis
		println("  time taken: "+niceTiming(after-before))
	}
	
	
	def operationsHelp:String = 
		"\n   == "+Decoy.opString+" ==\n"+ (new Decoy).usage + "\n" +
		"\n   == "+Isotopes.opString+" ==\n"+ (new Isotopes).usage + "\n" +
		"\n   == "+Include.opString+" ==\n"+ (new Include).usage + "\n" +
		"\n   == "+Clean.opString+" ==\n"+ (new Clean).usage + "\n" +
		"\n   == "+Subsample.opString+" ==\n"+ (new Subsample).usage + "\n" +
		"\n   == "+Stats.opString+" ==\n"+ (new Stats).usage + "\n"
	
	
	
	
	def niceTiming(t:Long) = {
		val ms = t % 1000
		var x = t / 1000
		val s = x % 60
		x = x / 60
		val m = x & 60
		x = x / 60
		val h = x % 24
		val d = x / 24
		"%d days %02d:%02d:%02d.%ds".format(d, h, m, s, ms)
	}
	
	
	def readInFile = {
		readMode match {
			case InCsv() =>
				CsvParser.fromFile(new BufferedReader(new FileReader(inFile)))
			case InTraml() =>
				GhostTraML.fromFile(new XmlReader(
					new BufferedReader(new FileReader(inFile))
				))
			case Unknown() =>
				println("WARN: unknown file extension, attemping to read as a .csv")
				CsvParser.fromFile(new BufferedReader(new FileReader(inFile)))
		}
	}
	
	
	def readModFile(f:File):Seq[IModifier] = {
		val r = new BufferedReader(new FileReader(f))
		var l = r.readLine()
		val mods = new ArrayBuffer[java.util.ArrayList[IModifier]]
		while (l != null) {
			mods += Modifier.fromString(l)
			l = r.readLine()
		}
		mods.flatten
	}
	
}