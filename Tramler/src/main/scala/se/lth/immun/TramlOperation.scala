package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._

abstract class TramlOperation(
		val name:String
) {
	var params:String = ""
	override def toString = name + (if (params != "") " " + params else "")
	
	def setup(params:String, mods:Seq[IModifier])
	def operate(in:GhostTraML):GhostTraML
	def usage:String
}