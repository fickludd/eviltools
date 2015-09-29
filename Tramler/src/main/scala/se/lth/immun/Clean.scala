package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._

object Clean {
	val opString = "clean"
}

class Clean extends TramlOperation("CLEAN") {
		
	def setup(params:String, mods:Seq[IModifier]) = {}
	
	def usage:String = """Remove all transitions and targets from the TraML. Keeps peptides and compounds. No options.
"""
	def operate(in:GhostTraML):GhostTraML = {
		in.includes.clear
		in.includeGroups.clear
		in.transitions.clear
		in.transitionGroups.clear
		in
	}
}