package se.lth.immun

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

class TsvWriter(f:File) {
	val w = new BufferedWriter(new FileWriter(f))
	
	def row(a:Array[Any]) =
			w.write(a.mkString("\t") + "\n")
	
	def row(a:Any*) =
			w.write(a.mkString("\t") + "\n")
			
	def close =
		w.close
}