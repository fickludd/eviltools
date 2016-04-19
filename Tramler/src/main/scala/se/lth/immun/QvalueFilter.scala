package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object QvalueFilter extends TramlOperation.Generator(
		"qvalueFilter",
		"""Filters proteins, AA-molecules and assays by qvalues. Needs file w. qvalues
    qvalues  file     File with qvalues in the Franklin format w. the levels Protein, 
                      AA-molecule and Assay (precursor charge). This option is required.
    mode     string   What level to filter on, protein, aamolecule, assay (default) or all 
    qvalue   float    The FDR threshold. 0.01 by default"""
) {

	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) =
		new QvalueFilterInstance(params, mods)
	
	trait Mode
	case object Protein extends Mode
	case object AAMolecule extends Mode
	case object Assay extends Mode
	case object All extends Mode
	
	case class Qval(id:String, qvalue:Double)
	case class QvalRow(protein:Qval, aaMolecule:Qval, assay:Qval)
	case class WhiteList(protein:Set[String], aaMolecule:Set[String], assay:Set[String])
	
	class QvalueFilterInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var qvalThreshold = 0.01
		var mode:Mode = Assay
		var file:Option[String] = None
		
		for ((k,v) <- params)
			k match {
				case "qvalues" 	=> file = Some(v)
				case "mode"		=> mode = 
					v match {
						case "protein" => Protein
						case "aaMolecule" => AAMolecule
						case "assay" => Assay
						case "all" => All
						case _ =>
							throw new IllegalArgumentException("Unknown qvalue filter mode '%s'".format(v))
					}
				case "qvalThreshold"		=> qvalThreshold = v.toDouble
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		if (file.isEmpty)
			throw new IllegalArgumentException("File with q-values not given. Cannot proceed!")
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			val whiteList = assembleWhiteList(parseQvalues(file.get))
			
			val out = new ClearTraML
			
			for (cp <- in.compounds) {
				val protGroup = cp.proteins.mkString(";")
				if (
					mode == AAMolecule || mode == Assay || 
					whiteList.protein.contains(protGroup)
				) {
					if (
						mode == Protein || mode == Assay ||
						whiteList.aaMolecule.contains(cp.id)
					) {
						val outCP = cp.metaCopy
						for (a <- cp.assays) {
							if (
								mode == Protein || mode == AAMolecule ||
								whiteList.assay.contains(assayID(cp.id, a.z.toString))
							) {
								val outAssay = outCP.getAssay(a.mz, a.z, a.ce)
								outAssay.ms1Channels ++= a.ms1Channels
								outAssay.ms2Channels ++= a.ms2Channels
							}
						}
						if (outCP.assays.nonEmpty) {
							out.compounds += outCP
							out.proteins += protGroup
						}
					}
				}
			}
			
			out
		}
		
		
		def assembleWhiteList(qvalues:Seq[QvalRow]) = {
			val proteins 	= Set.newBuilder[String]
			val aaMolecules = Set.newBuilder[String]
			val assays 		= Set.newBuilder[String]
			
			for (QvalRow(prot, aaMol, z) <- qvalues) {
				if (prot.qvalue < qvalThreshold) proteins += prot.id
				if (aaMol.qvalue < qvalThreshold) aaMolecules += aaMol.id
				if (z.qvalue < qvalThreshold) assays += assayID(aaMol.id, z.id)
			}
			
			WhiteList(proteins.result, aaMolecules.result, assays.result)
		}
		
		
		def parseQvalues(path:String) = {
			
			var parsedHeader = false
			val res = new ArrayBuffer[QvalRow]
			
			for (line <- Source.fromFile(new File(path)).getLines) {
				val p = line.split("\t").map(_.trim)
				if (!parsedHeader) {
					
					parsedHeader = true
				} else {
					res += QvalRow(
							Qval(p(0), p(3).toDouble), 
							Qval(p(4), p(7).toDouble), 
							Qval(p(8), p(11).toDouble)
						)
				}
			}
			
			res	
		}
		
		def assayID(aaMolecule:String, z:String) =
			aaMolecule + "_" + z
	}
}