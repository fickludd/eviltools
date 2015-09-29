package se.lth.immun.esv

import java.io.BufferedWriter

class EsvWriter(val e:Esv, val w:BufferedWriter) {

	import Esv._
	var sb = new StringBuilder
	sb ++= "# "+FILE_HEADER+"\n"
	sb ++= "#    separator: '"+e.separator+"'\n"
	sb ++= "#       escape: '"+e.escape+"'\n"
	if (e.sparse)
		sb ++= "#       sparse: true\n"
	if (e.nColumns.isDefined)
		sb ++= "#  num columns: "+e.nColumns.get+"\n"
	if (e.nRows.isDefined)
		sb ++= "#     num rows: "+e.nRows.get+"\n"
	sb ++= "#\n"
		
	if (e.origin) {
		sb ++= "# "+ORIGIN+"\n"
		if (e.source.isDefined)
			sb ++= "#         source program: "+e.source.get+"\n"
		if (e.version.isDefined)
			sb ++= "# source program version: "+e.version.get+"\n"
		if (e.author.isDefined)
			sb ++= "#                 author: "+e.author.get+"\n"
		if (e.mail.isDefined)
			sb ++= "#                   mail: "+e.mail.get+"\n"
		sb ++= "#\n"
	}
		
	if (e.hasParameters) {
		sb ++= "# "+PARAMETERS+"\n"
		for (kv <- e.parameters)
			sb ++= "# "+kv._1+": "+kv._2+"\n"
		sb ++= "#\n"
	}
	
	if (e.metaData.isDefined && !e.metaData.get.isEmpty) {
		for (s <- e.metaData.get)
			sb ++= "## "+s+"\n"
		sb ++= "#\n"
	}
	
	sb ++= "# "+COL_HEADERS+"\n"
	sb ++= "#: "+e.headers.mkString(e.separator+"")+"\n"
	
	w.write(sb.toString)
	
	
	def write(row:Seq[Any]) = 
		w.write(row.map(a => {
			val s = a.toString
			if (s.contains(e.separator))
				e.escape + s + e.escape
			else s
		}).mkString(e.separator+"")+"\n")
	
	
	
	def close = w.close
}