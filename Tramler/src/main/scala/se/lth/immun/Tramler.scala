package se.lth.immun

import se.jt.CLIApp

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
import scala.util.{Try, Success, Failure}

import se.lth.immun.traml.ghost._
import se.lth.immun.xml._
import se.lth.immun.chem._

object Tramler extends CLIApp {
	
	trait ReadMode
	case object InCsv extends ReadMode
	case object InTraml extends ReadMode
	case object Unknown extends ReadMode
	case object InFragmentBin extends ReadMode
	
	var modFile:File		= _
	var mods:Seq[IModifier] 	= Nil
	
	val properties = new Properties
    properties.load(this.getClass.getResourceAsStream("/pom.properties"))
    
	val t0 		= System.currentTimeMillis
	val name 	= properties.getProperty("pom.name")
	val version = properties.getProperty("pom.version")
    
	val params = new TramlerParams
    
	val allowedOperations = Array(Clean, Decoy, Isotopes, Subsample, Stats, Trim)
	
	def main(args:Array[String]):Unit = {
		
		val argsErrs = parseArgs(name, version, args, params, List("in"), Some("ops"))
		if (argsErrs.nonEmpty) {
			println("\n\n  OPERATIONS:")
			println("  ===========")
			println("    operations syntax is 'operation(option1,option2=value,...,optionN)'\n")
			println(operationsHelp)
			failOnError(argsErrs)
		}
    	    	
		println(name + " "+version)
		println("       input file: "+params.in.value)
    	
		
		val (readMode, outDef1) = inFormat(params.inFile)
		
		val ops = params.ops.split(" ").map(parseOp)
		val outputDefault = 
			if (params.ops.split(" ").exists(_.startsWith(Decoy.opString)))
				if (isExt(outDef1, ".out")) 
					replaceExt(outDef1, ".traml.out", ".decoy.traml")
				else
					replaceExt(outDef1, ".traml", ".decoy.traml")
			else outDef1
		
		var traml = readFile(readMode, params.inFile)
    	
		for (op <- ops) {
			println("  [ %s ]".format(op.toString))
			traml = op.operate(traml, params)
		}
    	
		val outFile = params.outFile.getOrElse(outputDefault)
		println("    [OUTPUT] file: "+outFile)
    	println()
    	
    	traml.write(XmlWriter(new BufferedWriter(new FileWriter(outFile))))
		
		println("  time taken: "+niceTiming(System.currentTimeMillis - t0))
	}
	
	
	
	def operationsHelp:String = 
		allowedOperations.mkString("\n\n")
	
	
		
	def parseOp(str:String):TramlOperation.Instance = {
		def getGen(op:String) = 
			allowedOperations.find(_.opString == op) match {
				case Some(gen) => gen
				case None =>
					throw new IllegalArgumentException("'%s' is not a valid traml operation!".format(str))
			}
		
		def getParams(str:String):Seq[(String, String)] = 
			str.split(",").map(kv => 
				if (kv.contains("=")) {
					val parts = kv.split("=", 2)
					(parts(0), parts(1))
				} else (kv, "")
			)
		
		val (gen, params) = 
			if (str.contains("(")) {
				val parts 	= str.split("\\(",2)
				(getGen(parts(0)), getParams(parts(1).init))
			} else 
				(getGen(str), Nil)
		
		Try(gen.makeInstance(params, mods)) match {
			case Success(inst) => inst
			case Failure(e) =>
				throw new IllegalArgumentException(gen.toString, e)
		}
	}
		
	
	def inFormat(inFile:File) = {
		if (isExt(inFile, ".traml"))
			(InTraml, appendExt(inFile, ".out"))
		else if (isExt(inFile, ".csv") || 
					isExt(inFile, ".txt") || 
					isExt(inFile, ".tsv")
				)
			(InCsv, replaceExt(inFile, ".csv", ".traml"))
		else if (isExt(inFile, ".fragments.bin"))
			(InFragmentBin, appendExt(inFile, ".traml"))
		else
			(Unknown, appendExt(inFile, ".traml"))
	}
		
	
	
	def readFile(readMode:ReadMode, f:File) = {
		readMode match {
			case InCsv =>
				CsvParser.fromFile(new BufferedReader(new FileReader(f)))
			case InTraml =>
				GhostTraML.fromFile(new XmlReader(
					new BufferedReader(new FileReader(f))
				))
			case InFragmentBin =>
				FragmentBin.parse(f)
			case Unknown =>
				println("WARN: unknown file extension, attemping to read as a .csv")
				CsvParser.fromFile(new BufferedReader(new FileReader(f)))
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
	
	
	def isExt(f:File, ext:String) =
		f.toString.toLowerCase.endsWith(ext)
	
	def replaceExt(f:File, oldExt:String, ext:String) =
		new File(f.toString.dropRight(oldExt.length) + ext)
	
	def appendExt(f:File, ext:String) =
		new File(f.toString + ext)
}