package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.traml.clear.Clear._
import se.lth.immun.chem._
import se.lth.immun.unimod.UniMod
import se.lth.immun.xlink.XLink

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection.mutable.Queue
import util.Random

object Decoy extends TramlOperation.Generator(
		"decoy", 
		"""  Replace peptides and compounds with similar fakes. 
    novel            generate new peptides based on aa frequencies
    shuffle          generate new peptides by shuffling aas, except c-term and modified n-term aas. (default)
    reverse          generate new peptides by reversing aas, except c-term and modified n-term aas.
    same             reuse the same peptides (fake fakes).
    decoy-factor     integer stating how many decoys should be generated per compound / peptide (novel and shuffle only).
    n                integer stating how many decoy peptides and compounds that should be generated in total.
    seed             seed for decoy sub-selection.
    verbose          output a lot on std out."""
) {
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new DecoyInstance(params, mods)
	
	import Subsample._
	
	trait DecoyLevel
	case object Protein extends DecoyLevel
	case object Compound extends DecoyLevel
	
	class DecoyInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var getDecoyPep = shuffle _
		var mode:SubSampleMode = Fraction(1)
		var seed:Option[Long] = None
		var verbose = false
		var level:DecoyLevel = Compound
		var prefix = "DECOY_"
		val timer = new Timer
		
		for ((k,v) <- params)
			k match {
				case "novel" 		=> getDecoyPep = novel _
				case "shuffle" 		=> getDecoyPep = shuffle _
				case "reverse" 		=> getDecoyPep = reverse _
				case "same" 		=> getDecoyPep = same _
				case "decoy-factor" => mode = Fraction(v.toDouble)
				case "n" 			=> mode = N(v.toInt)
				case "seed"			=> seed = Some(v.toLong)
				case "verbose"		=> verbose = true
				case "prefix"		=> prefix = v
				case "level"		=> 
					level = 
						v match {
							case "protein" => Protein
							case "compound" => Compound
							case "peptide" => Compound
							case x => throw new IllegalArgumentException("Unknown decoy level '%s'".format(x))
						}
				case _ =>
					throw new IllegalArgumentException("Unknown param '%s'".format(k))
			}
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			timer.reset
			
			val out = new ClearTraML
	    	
	    	import PeptideParser._
	    	
			level match {
				case Compound =>
					out.compounds ++= 
			    		subsample(in.compounds, mode, seed).map(cp => {
			    			parseSequence(cp.id) match {
			    				case XLinkPeptide(xl) => toXLinkDecoy(xl, cp)
			    				case UniModPeptide(p) => toUniModDecoy(p, cp)
			    			}
			    		})
		    		
		    		out.proteins ++= out.compounds.flatMap(_.proteins)
		    	
				case Protein =>
					val prots = subsample(in.proteins.toSeq, mode, seed)
					out.proteins ++= prots.map(p => prefix + p)
					
					for {
						cp <- in.compounds
						if cp.proteins.exists(out.proteins.contains)
					} {
						out.compounds +=
							(parseSequence(cp.id) match {
			    				case XLinkPeptide(xl) => toXLinkDecoy(xl, cp)
			    				case UniModPeptide(p) => toUniModDecoy(p, cp)
			    			})
					}
			}
			
	    	
	    		
	    	/*for ((ref, comp) <- in.compounds)
	    		addCompoundDecoys(in, out, ref, comp)
	    	val pepTransTable = in.transitionGroups.keys.groupBy(_.pepCompId)
	    	
	    	import PeptideParser._
	    	for ((ref, pep) <- in.peptides) {
	    		parseSequence(pep.sequence) match {
	    			case XLinkPeptide(xl) =>
	    				addXLinkDecoys(in, out, xl, ref, pep, pepTransTable)
	    			case UniModPeptide(p) =>
	    				addUniModDecoys(in, out, p, ref, pep, pepTransTable)
	    		}
	    	}
	    	*/
	    	
	    	out
		}
		
		
		/*
		def addCompoundDecoys(in:GhostTraML, out:GhostTraML, ref:String, comp:GhostCompound) = {
			
			val allTransitions = in.transitions.filter(_.isCompound(ref))
			val allTargets = in.includeGroups.getOrElse(ref, new ArrayBuffer)
			val q1zs = 
				if (comp.preferredCharges.nonEmpty) comp.preferredCharges
				else	allTransitions.map(_.q1z).toSet ++ allTargets.map(_.q1z)
			
			for (q1z <- q1zs) {
				val trans = allTransitions.filter(_.q1z == q1z)
				val targets = allTargets.filter(_.q1z == q1z)
				val q1 = (trans.map(_.q1) ++ targets.map(_.q1)).min
				
				for (i <- 0 until decoyFactor) {
					val decoyC 	= getDecoyComp(comp)
					val decoyCompRef = "DECOY_"+i+"_"+ref
					decoyC.id = decoyCompRef
					val dq1 	= (decoyC.mass + q1z * Constants.PROTON_WEIGHT) / q1z
					out.compounds(decoyCompRef) = decoyC
					
	    			for (j <- 0 until targets.length) {
	    				val gt = new GhostTarget
	    				gt.id = "DECOY_"+i+"_"+targets(j).id
	    				gt.compound = Some(decoyC)
	    				gt.q1 = dq1 + (targets(j).q1 - q1)
	    				gt.q1z = q1z
	    				gt.localRT = targets(j).localRT
	    				gt.intensity = targets(j).intensity
	    				out += gt
	    			}
				}
			}
		}
		*/
		
		
		def toXLinkDecoy(xl:XLink, cp:ClearPeptide):ClearPeptide = {
			throw new Exception("Xlink decoy generation is not yet possible!")
		}
		
		
		
		def toUniModDecoy(p:Peptide, cp:ClearPeptide):ClearPeptide = {
			//var p = UniMod.parseUniModSequence(pep.sequence)
			//timer.click
			//val pm = p.monoisotopicMass
			
			import EPeptideFragment._
			val decoyP 	= Modifier.modify(getDecoyPep(p), mods.toArray)
			val decoyFrags = decoyP.getFragments(Array(a, b, c, x, y, z))
    		val dm 		= decoyP.monoisotopicMass
    		val decoyCompound = new ClearPeptide(decoyP.toString, cp.proteins.map(prefix+_))
    		decoyCompound.rt = cp.rt
			
			for (a <- cp.assays) {
				
				val q1 = dm / a.z + Constants.PROTON_WEIGHT
				val decoyAssay = 
					decoyCompound.getAssay(q1, a.z, a.ce)
				
				for (c <- a.ms1Channels)
					decoyAssay.ms1Channels += 
						Channel(
							(dm / c.z + Constants.PROTON_WEIGHT) + (c.mz - q1),
							c.z,
							c.id,
							1,
							c.expIntensity
						)
				
				decoyAssay.ms2Channels ++= a.ms2Channels.map(reinterpretMs2Channel(_, decoyP, decoyFrags))
			}
	    	
	    	//val dt4 = timer.click
	    	
	    	//println("%8d %8d %8d %8d".format(dt1, dt2, dt3, dt4))
	    	decoyCompound
		}
		
		
		
		val regularIonRE = """([abcxyz])([0-9]+)""".r.unanchored
		val internalIonRE = """m([0-9]+):([0-9]+)""".r.unanchored
		def reinterpretMs2Channel(ch:Channel, p:Peptide, frags:Seq[PeptideFragment]):Channel = {
			ch.id match {
				case regularIonRE(frag, ordinal) =>
					val ePepFrag = EPeptideFragment.fromChar(frag.head)
					frags.find(f => f.fragmentType == ePepFrag && f.ordinal == ordinal.toInt) match {
						case Some(pepFrag) =>
							Channel(
								pepFrag.monoisotopicMass / ch.z + Constants.PROTON_WEIGHT,
								ch.z,
								ch.id,
								2,
								ch.expIntensity
							)
						case None =>
							throw new Exception("Fragment '%s' not parsable in peptide %s".format(ch.id, p.toString))
							
					}
				case internalIonRE(start, stop) =>
					val aas = p.aminoAcids
					val m = aas.slice(start.toInt, stop.toInt+1).map(_.monoisotopicMass).sum
					Channel(
						m / ch.z + Constants.PROTON_WEIGHT,
						ch.z,
						ch.id,
						2,
						ch.expIntensity
					)
				case x:String =>
					throw new Exception("Unparsable fragment '%s' in %s".format(x, p.toString))
			}
		}
	}
	
	
	def shuffle(p:Peptide):Peptide = {
		var shuffled =
			p.aminoAcids.head match {
				case saa:StandardAminoAcid =>
					Random.shuffle(p.aminoAcids.dropRight(1).toList) :+ p.aminoAcids.last
				case aa:IAminoAcid =>
					p.aminoAcids.head +: Random.shuffle(p.aminoAcids.dropRight(1).drop(1).toList) :+ p.aminoAcids.last
			}
		if (shuffled.zip(p.aminoAcids).forall(t => t._1 == t._2))
			shuffle(p)
		else
			new Peptide(shuffled.toArray)
	}
	
	def novel(p:Peptide) = {
		import StandardAminoAcid._
		
		def getAA:StandardAminoAcid = {
			val r = math.random * 100
			var cumsum = 0.0
			for ((aa, freq) <- aaFreq) {
				cumsum += freq
				if (r < cumsum)
					return aa
			}
			return aaFreq.last._1//throw new Exception("This definitely shouldn't happen!")
		}
		
		val m = p.monoisotopicMass
		val aas = new ArrayBuffer[StandardAminoAcid]
		aas += (if (math.random * (5.84+5.53) < 5.84) K else R)
		
		var m2 = 0.0
		while (m2 < m) {
			val aa = getAA
			m2 += aa.monoisotopicMass
			aas += aa
		}
		
		m2 += Constants.WATER_WEIGHT
		if (math.abs((m2 - aas.last.monoisotopicMass) - m) < math.abs(m2 - m))
			new Peptide(aas.init.reverse.toArray)
		else
			new Peptide(aas.reverse.toArray)
	}
	
	
	def reverse(p:Peptide) = {
		new Peptide(p.aminoAcids.dropRight(1).reverse :+ p.aminoAcids.last)
	}
	
	
	def same(p:Peptide) = p
	
	/**
	 * AA frequencies in entire UniProtKB according to 
	 * http://web.expasy.org/docs/relnotes/relstat.html
	 */
	val aaFreq = {
		import StandardAminoAcid._
		Array(
			Tuple2(A, 8.25),
			Tuple2(Q, 3.93),
			Tuple2(L, 9.66),
			Tuple2(S, 6.56),
			Tuple2(R, 5.53),
			Tuple2(E, 6.75),
			Tuple2(K, 5.84),
			Tuple2(T, 5.34),
			Tuple2(N, 4.06),
			Tuple2(G, 7.07),
			Tuple2(M, 2.42),
			Tuple2(W, 1.08),
			Tuple2(D, 5.45),
			Tuple2(H, 2.27),
			Tuple2(F, 3.86),
			Tuple2(Y, 2.92),
			Tuple2(C, 1.37),  
			Tuple2(I, 5.96),
			Tuple2(P, 4.70),
			Tuple2(V, 6.87))
	}
}