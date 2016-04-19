package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

object Tsv extends TramlOperation.Generator(
			"tsv",
			""" Output the current traML transitions as a tsv.
    name             output file to write to. Will default to 'out.tsv'
    mode    string   either 'default' or 'skyline'"""
) {
	
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new TsvInstance(params, mods)

	
	trait Mode {
		def write(in:ClearTraML, outName:String):Unit = {
			val w = new BufferedWriter(new FileWriter(new File(outName)))
			def row(xs:Any*) = 
				w.write(xs.map(_.toString).mkString("\t")+"\n")
				
			minorWrite(in, row)
			w.close()
		}
		def minorWrite(in:ClearTraML, row:(Any*) => Unit):Unit
	}
	case object Default extends Mode {
		def minorWrite(in:ClearTraML, row:(Any*) => Unit) = {
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
		}
	}
	case object Skyline extends Mode {
		def minorWrite(in:ClearTraML, row:(Any*) => Unit) = {
			row("Protein", "Peptide", "PrecursorMz", "PrecursorCharge", "ProductMz", "ProductCharge", "Tr_recalibrated", "LibraryIntensity", "decoy", "FragmentIon")
			
			for {
				cp <- in.compounds
				a <- cp.assays
				c <- a.ms2Channels
			}
				row(
					cp.proteins.mkString(";"),
					cp.id,
					a.mz, 
					a.z, 
					c.mz, 
					c.z, 
					cp.rt.map(_.t).getOrElse("-"),
					c.expIntensity.getOrElse("-"), 
					false, 
					c.id
				)
		}
	}
	
	class TsvInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var outName = "out.tsv"
		var mode:Mode = Default
			
		for ((k,v) <- params)
			k match {
				case "name"		=> outName = v
				case "mode"		=> 
					v match {
						case "default" => mode = Default
						case "skyline" => mode = Skyline
						case x => 
							throw new IllegalArgumentException("unknown tsv mode '%s'".format(x))
					}
			}
		
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			mode.write(in, outName)
			in
		}
	}
}