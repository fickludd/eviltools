package se.lth.immun

import scala.collection.mutable.HashSet

import se.lth.immun.traml.ghost._

class GhostTramlBuilder {

	val x 		= new GhostTraML
    val ts 		= new HashSet[GhostTransition]
	
	def addProtein(id:String, acc:Option[String]) = {
		if (!x.proteins.contains(id)) {
			val gProt = new GhostProtein
	    	gProt.id 		= id
	    	gProt.accession = acc.getOrElse("unknown")
	    	gProt.name 		= id 
			gProt.shortName = id
			gProt.sequence 	= ""
	    	x.proteins += gProt.id -> gProt
		}
	}
	
	def addPeptide(seq:String) = {
		if (!x.peptides.contains(seq)) {
			val gPep = new GhostPeptide
			gPep.id 		= seq
			gPep.sequence 	= seq
			x.peptides += gPep.id -> gPep
		}
	}
	
	def setPepProt(pepSeq:String, protId:String) = 
		x.peptides(pepSeq).proteins += protId 
	
	
	def addTransition(gt:GhostTransition) = 
		if (!ts.contains(gt))
    		ts += gt
	
    def result = {
		for (gt <- ts) 
			x += gt
		x
	}
    	
}