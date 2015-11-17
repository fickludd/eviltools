package se.lth.immun

import scala.collection.mutable.HashMap

import se.lth.immun.traml.clear._

class ClearTraMLBuilder {

	val x = new ClearTraML
	val mapping = new HashMap[String, ClearPeptide]
	
	def addProtein(id:String) = {
		x.proteins += id
		id
	}
	
	def addPeptide(seq:String, proteins:Seq[String] = Nil) = {
		if (!mapping.contains(seq)) {
			val cp = new ClearPeptide(seq, proteins)
			mapping(seq) = cp
			x.compounds += cp
		}
		mapping(seq)
	}
	
	def setPepProt(pepSeq:String, protId:String) = 
		mapping(pepSeq).proteins += protId 
	
    def result = x
    	
}