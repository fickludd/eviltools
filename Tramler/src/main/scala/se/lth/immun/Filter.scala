package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import java.io.File
import scala.io.Source

object Filter extends TramlOperation.Generator(
		"filter",
		"""Filters proteins and AA-molecules by name. Needs file w. protein tags or AA-molecules, one entry per row.
    proteinTags     file    File with protein tags to keep. Protein names that contain at least one tag will be kept. 
    aaMolecules     file    File with AA-molecules to keep. AA-molecules exactly matching a file entry will be kept. """
) {

	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) =
		new FilterInstance(params, mods)
	
	class FilterInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var proteinTagFile:Option[String] = None
		var aaMoleculeFile:Option[String] = None
		
		for ((k,v) <- params)
			k match {
				case "proteinTags" 	=> proteinTagFile = Some(v)
				case "aaMolecules"	=> aaMoleculeFile = Some(v)
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		if (proteinTagFile.isEmpty && aaMoleculeFile.isEmpty)
			throw new IllegalArgumentException("Need one of 'proteinTags' or 'aaMolecules' for filtering!")
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			val proteinTags = proteinTagFile.map(parseFilter)
			val aaMolecules = aaMoleculeFile.map(parseFilter)
			
			val out = new ClearTraML
			
			for (cp <- in.compounds) {
				if (
					proteinTags.isEmpty || 
					proteinTags.get.exists(tag => cp.proteins.exists(_.indexOfSlice(tag) > 0))
				) {
					if (
						aaMolecules.isEmpty ||
						aaMolecules.get.contains(cp.id)
					) {
						out.compounds += cp
						out.proteins += cp.proteins.mkString(";")
					}
				}
			}
			
			out
		}
		
	}
	
	def parseFilter(path:String) = {
		Source.fromFile(new File(path)).getLines.toSeq
	}

}