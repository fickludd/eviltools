package se.lth.immun

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

class TsvWriter(f:File) {
	val w = new BufferedWriter(new FileWriter(f))
	
	def row(a:Array[Any]) =
			w.write(a.mkString("\t") + "\n")
	
	def row(vals:Any*) = {
		val ab = Array.newBuilder[Any]
		for (a <- vals) 
			a match {
				case s:Seq[Any] => ab ++= s
				case a:Any => ab += a
			}
		w.write(ab.result.mkString("\t") + "\n")
	}
	
	def close =
		w.close
}