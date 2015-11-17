package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

object Clean extends TramlOperation.Generator(
		"clean",
		"""  Remove all transitions and targets from the TraML. Keeps peptides and compounds.
  mode         'assay'    removes all info except peptides
               'channel'  removes all ms1 and ms2 channels but leaves empty assays
               'ms1'      removes all ms1 channels
               'ms2'      removes all ms2 channels"""
) {
	import TramlOperation._
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new CleanInstance(params)
	
	trait CleanMode
	case object Assay extends CleanMode
	case object Channel extends CleanMode
	case object Ms1 extends CleanMode
	case object Ms2 extends CleanMode
	
	class CleanInstance(params:Seq[(String, String)]) extends Instance(opString, params) {
		
		var mode:CleanMode = Assay
		
		for ((k,v) <- params)
			k match {
				case "mode" => 
					v match {
						case "assay" => mode = Assay
						case "channel" => mode = Channel
						case "ms1" => mode = Ms1
						case "ms2" => mode = Ms2
						case _ => 
							throw new IllegalArgumentException("Unknown clean mode '%s'".format(v))
					}
				case _ =>
					throw new IllegalArgumentException("Unknown param '%s'".format(k))
			}
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			val out = new ClearTraML
			out.proteins ++= in.proteins
			for (cp <- in.compounds) {
				val outCP = new ClearPeptide(cp.id, cp.proteins)
				outCP.rt = cp.rt
				if (mode != Assay) 
					for (a <- cp.assays) {
						val outAssay = outCP.getAssay(a.mz, a.z, a.ce)
						if (mode == Ms1) 
							outAssay.ms2Channels ++= a.ms2Channels
						else if (mode == Ms2) 
							outAssay.ms1Channels ++= a.ms1Channels
					}
				
				out.compounds += outCP
			}
			out
		}
	}
}