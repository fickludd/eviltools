package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object Tsv extends TramlOperation.Generator(
			"tsv",
			""" Output the current traML transitions as a tsv.
	name       output file to write to. Will default to 'out.tsv'"""
) {
	
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new TsvInstance(params, mods)

	
	class TsvInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var outName = "out.tsv"
		
		for ((k,v) <- params)
			k match {
				case "name" 		=> outName = v
			}
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			
			val w = new BufferedWriter(new FileWriter(new File(outName)))
			def row(xs:Any*) = 
				w.write(xs.map(_.toString).mkString("\t")+"\n")
				
			row("peptide", "precursorMz", "precursorCharge", "fragmentMz", "fragmentCharge", "expIntensity", "expRT", "fragmentIon")
			
			for {
				cp <- in.compounds
				a <- cp.assays
				c <- a.ms2Channels
			}
				row(
					cp.id, 
					a.mz, 
					a.z, 
					c.mz, 
					c.z, 
					c.expIntensity.getOrElse("-"), 
					cp.rt.map(_.t).getOrElse("-"), 
					c.id
				)
			
			w.close
			
			in
		}
	}
}