package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._

object Clean extends TramlOperation.Generator(
		"clean",
		"""  Remove all transitions and targets from the TraML. Keeps peptides and compounds."""
) {
	import TramlOperation._
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new CleanInstance(params)
	
	class CleanInstance(params:Seq[(String, String)]) extends Instance(opString, params) {
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			in.includes.clear
			in.includeGroups.clear
			in.transitions.clear
			in.transitionGroups.clear
			in
		}
	}
}