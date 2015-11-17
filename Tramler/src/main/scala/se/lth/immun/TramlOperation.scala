package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

object TramlOperation {
	abstract class Generator(
			val opString:String,
			val usage:String
	) {
		def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]):Instance
		//override def toString = name + (if (params != "") " " + params else "")
		
		override def toString = "%s\n%s\n%s".format(opString, opString.map(_ => '-').mkString, usage)
	}
	
	abstract class Instance(opString:String, params:Seq[(String, String)]) {
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML
		
		override def toString =
			"%s(%s)".format(opString.toUpperCase, params.map(t => 
				if (t._2 != "") t._1 + "=" + t._2
				else t._1
				).mkString(","))
	}
}