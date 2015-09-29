package se.lth.immun.esv

import scala.collection.mutable.ArrayBuffer

object Esv {
	
	class FormatException(mess:String, line:Int) extends Exception("Line %d".format(line) + mess) {}
	class UserException(mess:String) extends Exception(mess)
	
	val FILE_HEADER = "EXTENDED SEPARATED VALUES"
	val SEP = "separator"
	val ESC = "escape"
	val SPARSE = "sparse"
	val NUM_COLUMNS = "num columns"
	val NUM_ROWS = "num rows"
	val ORIGIN = "ORIGIN"
	val PARAMETERS = "PARAMETERS"
	val COL_HEADERS = "COLUMN HEADERS"
	val SOURCE = "source program"
	val VERSION = "source program version"
	val AUTHOR = "author"
	val MAIL = "mail"
	val VALUE_TEMPLATE = "<value1_1><separator><value1_2><separator>...<value1_N>"
}



class Esv {
	//File info
	var separator = '\t'
	var escape = '"'
	var sparse = false
	var nColumns:Option[Int] = None
	var nRows:Option[Int] = None
	
	//Origin
	var source:Option[String] = None
	var version:Option[String] = None
	var author:Option[String] = None
	var mail:Option[String] = None
	
	//Rest
	private var _paras = new ArrayBuffer[(String, String)]
	var metaData:Option[Seq[String]] = None
	var headers:Seq[String] = Nil
	
	def origin = source.isDefined || version.isDefined || author.isDefined || mail.isDefined
	
	def hasParameters = _paras.nonEmpty
	def addParameter(name:String, value:Any) = _paras += name -> value.toString
	def getParameter(name:String):Option[String] = _paras.find(_._1 == name).map(_._2)
	def parameters:Seq[(String, String)] = _paras.clone
}