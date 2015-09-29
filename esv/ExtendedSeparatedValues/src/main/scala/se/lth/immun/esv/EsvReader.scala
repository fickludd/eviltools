package se.lth.immun.esv

import java.io.BufferedReader

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class EsvReader(val r:BufferedReader) extends Esv {

	import Esv._
	
	// Internal reading 
	var line = ""
	var lineNo = 0
	var isMeta = false
	var metaOver = false
	var values:Seq[String] = null
	var EOF = false
	def readLine = {
		line = r.readLine
		if (line == null)
			EOF = true
		else {
			if (line(0) == '#') {
				isMeta = true
				line = line.tail.trim
			} else
				isMeta = false
			lineNo += 1
			if (metaOver)
				values = readRow(line)
		}
	}
	
	
	
	def close = r.close
	
	
	def getValue(header:String) = 
		if (headers.contains(header))	values(headers.indexOf(header))
		else throw new UserException(
			"Header '%s' not among read headers: %s".format(header, headers.mkString(", ")))
	
	
	
	private def readRow(line:String):Seq[String] = {
		
		def clean(w:String) = {
			var v = w.trim
			if (v.nonEmpty && v(0) == escape) v = v.tail
			if (v.nonEmpty && v.last == escape) v = v.init
			v
		}
		var words = new ArrayBuffer[String]
		
		if (line == null || line == "") return words
		var wordStart = 0
		var isImmune = false
		var stripChars = Array(' ', '\t', '\n', '\r', escape)
		
		for (i <- 0 until line.length) {
			var c = line.charAt(i)
			if (c == separator && !isImmune) {
				words += clean(line.substring(wordStart, i))
				wordStart = i + 1
			}
			else if (c == escape)
				isImmune = !isImmune
		}
		words += clean(line.substring(wordStart, line.length))
		
		return words
	}
	
	
	
	private def error(str:String, got:String, wanted:String) = 
		throw new FormatException(str+", got '%s' wanted '%s'".format(got, wanted), lineNo)
	
	private def readParam(p:String, fail:Boolean = false):Option[String] = {
		try {
			var s = line.split(":", 2)
			if (s(0).trim == p) 
				Some(s(1).trim)
			else
				if (fail) error("Failed parsing param", line, "# %s: <value>".format(p))
				else None
		} catch {
			case _:Throwable => 
				if (fail) error("Failed parsing param", line, "# %s: <value>".format(p))
				else None
		}
	}
	
	private def parseFileHeader = {
		readLine
		separator = readParam(SEP, true).get(1)
		readLine
		escape = readParam(ESC, true).get(1)
		readLine
		readParam(SPARSE, false) match {
			case Some(s) => {
				sparse = s == "true"
				readLine
			}
			case None => sparse = false
		}
		readParam(NUM_COLUMNS, false) match {
			case Some(s) => {
				nColumns = Some(s.toInt)
				readLine
			}
			case None => nColumns = None
		}
		readParam(NUM_ROWS, false) match {
			case Some(s) => {
				nRows = Some(s.toInt)
				readLine
			}
			case None => nRows = None
		}
	}
	
	
	
	private def parseOrigin = {
		readLine
		readParam(SOURCE, false) match {
			case Some(s) => {
				source = Some(s)
				readLine
			}
			case None => sparse = false
		}
		readParam(VERSION, false) match {
			case Some(s) => {
				version = Some(s)
				readLine
			}
			case None => nColumns = None
		}
		readParam(AUTHOR, false) match {
			case Some(s) => {
				author = Some(s)
				readLine
			}
			case None => nRows = None
		}
		readParam(MAIL, false) match {
			case Some(s) => {
				mail = Some(s)
				readLine
			}
			case None => nRows = None
		}
	}
	
	
	
	private def parseParameters = {
		while ({readLine; line.length > 1}) {
			var s = line.split(":", 2)
			addParameter(s(0).trim, s(1).trim)
		}
	}
	
	
	
	private def parseMetaData = {
		val a = new ArrayBuffer[String]
		a += line.tail.trim
		while ({readLine; line.startsWith("#")}) {
			a += line.tail.trim
		}
		metaData = Some(a)
	}
	
	
	
	readLine
	if (line != FILE_HEADER)
		error("Incorrect file header line", line, "# "+FILE_HEADER)
	parseFileHeader
		
	while ({readLine; isMeta && !metaOver}) {
		if (line.isEmpty) {}
		else if (line == ORIGIN) 			parseOrigin
		else if (line == PARAMETERS) 		parseParameters
		else if (line.startsWith("#")) 		parseMetaData
		else if (line == COL_HEADERS) {
			readLine
			headers = readRow(line.tail)
			metaOver = true
		}
	}
	
	if (isMeta)
		error("Got meta info after column headers", line, VALUE_TEMPLATE)
		
	if (!metaOver)
		throw new FormatException("Stopped reading meta without reading headers", lineNo)
	
}
