package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._
import se.lth.immun.unimod.UniMod
import se.lth.immun.xlink.XLink

import collection.mutable.ArrayBuffer
import collection.mutable.Queue
import util.Random

object Decoy extends TramlOperation.Generator(
		"decoy", 
		"""  Replace peptides and compounds with similar fakes. 
    novel            generate new peptides based on aa frequencies
    shuffle          generate new peptides by shuffling aas, except c-term and modified n-term aas.
    reverse          generate new peptides by reversing aas, except c-term and modified n-term aas.
    same             reuse the same peptides (fake fakes).
    decoy-factor     integer stating how many decoys should be generated per compound / peptide (novel and shuffle only).
    n                integer stating how many decoy peptides and compounds that should be generated in total."""
) {
		
	import Decoy._
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new DecoyInstance(params, mods)
	
	class DecoyInstance(
			params:Seq[(String, String)], 
			val mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var getDecoyPep = shuffle _
		var decoyFactor = 1
		var nDecoys = -1
		
		for ((k,v) <- params)
			k match {
				case "novel" 		=> getDecoyPep = novel _
				case "shuffle" 		=> getDecoyPep = shuffle _
				case "reverse" 		=> getDecoyPep = reverse _
				case "same" 		=> getDecoyPep = same _
				case "decoy-factor" => decoyFactor = v.toInt
				case "n=" 			=> nDecoys = v.toInt
				case _ =>
					throw new IllegalArgumentException("Unknown param '%s'".format(k))
			}
		
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			val out = new GhostTraML
	    	val gProt = new GhostProtein
	    	gProt.id 		= "decoy"
	    	gProt.accession = "decoy"
			gProt.name 		= "decoy" 
			gProt.shortName = "decoy"
			gProt.sequence 	= ""
	    	out.proteins += gProt.id -> gProt
	    	
	    	for ((ref, comp) <- in.compounds)
	    		addCompoundDecoys(in, out, ref, comp)
	    	    	
	    	import PeptideParser._
	    	for ((ref, pep) <- in.peptides) {
	    		PeptideParser.parseSequence(pep.sequence) match {
	    			case XLinkPeptide(xl) =>
	    				addXLinkDecoys(in, out, xl, ref, pep)
	    			case UniModPeptide(p) =>
	    				addUniModDecoys(in, out, p, ref, pep)
	    		}
	    	}
	    	
	    	out
		}
		
		
		
		def findIons(q3s:Seq[Double], peptide:Peptide):Seq[Ion[PeptideFragment]] = {
			val fs = peptide.getFragments(Array(EPeptideFragment.y, EPeptideFragment.b))
			var fsz = (for {
					f <- fs
					z <- 1 until 4
				} yield (f, z)).toSet
			var retFs = new ArrayBuffer[Ion[PeptideFragment]]
			q3s.map(q3 => {
				var diff = Double.MaxValue
				var best:PeptideFragment = null
				var bestz = 0
				for ((f,z) <- fsz) {
					if (math.abs(f.mass - (z*q3 - z)) < diff) {
						diff = math.abs(f.mass - (z*q3 - z))
						best = f
						bestz = z
					}
				}
				if (best != null) {
					retFs += new Ion(best, bestz)
					fsz -= best -> bestz
				}
			})
			return retFs
		}
		
		
		
		def addCompoundDecoys(in:GhostTraML, out:GhostTraML, ref:String, comp:GhostCompound) = {
			
			val allTransitions = in.transitions.filter(_.compoundRef == ref)
			val allTargets = in.includes.filter(_.compoundRef == ref)
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
	    				gt.compoundRef = decoyCompRef
	    				gt.q1 = dq1 + (targets(j).q1 - q1)
	    				gt.q1z = q1z
	    				gt.rtStart = targets(j).rtStart
	    				gt.rtEnd = targets(j).rtEnd
	    				gt.intensity = targets(j).intensity
	    				out += gt
	    			}
				}
			}
		}
		
		
		
		def addXLinkDecoys(in:GhostTraML, out:GhostTraML, xl:XLink, ref:String, pep:GhostPeptide) = {
			
		}
		
		
		
		def addUniModDecoys(in:GhostTraML, out:GhostTraML, p:Peptide, ref:String, pep:GhostPeptide) = {
			//var p = UniMod.parseUniModSequence(pep.sequence)
			val allTransitions = in.transitions.filter(_.peptideRef == ref)
			val allTargets = in.includes.filter(_.peptideRef == ref)
			val q1zs = allTransitions.map(_.q1z).toSet ++ allTargets.map(_.q1z)
			
			for (q1z <- q1zs) {
				val trans = allTransitions.filter(_.q1z == q1z)
				val targets = allTargets.filter(_.q1z == q1z)
				val q1 = (trans.map(_.q1) ++ targets.map(_.q1)).min
				val ions = findIons(trans.map(_.q3), p)
				
	    		try {
	    			println("%8.3f %s | %s".format(q1, p.toString, 
	    					ions.map(i => i.molecule.fragmentType+""+i.molecule.ordinal).mkString(" ")))
	    		} catch {
	    			case e:Exception => {
	    				println("ERROR IN peptide %8.3f %s | %s".format(q1, p.toString, trans.map(x => "%.3f".format(x.q3)).mkString(" ")))
					e.printStackTrace
	    			}
	    		}
	    		
	    		for (i <- 0 until decoyFactor) {
	    			val decoyP 	= Modifier.modify(getDecoyPep(p), mods.toArray)
	    			val decoyPepRef = "DECOY_"+i+"_"+ref
	    			out.peptides += decoyPepRef -> toGhostPeptide(decoyPepRef, decoyP, pep)
	    			val dm 		= decoyP.monoisotopicMass
	    			val dq1mz 	= (dm + q1z * Constants.PROTON_WEIGHT) / q1z
	    			val dq1 	= Ion.mz(decoyP, q1z)
	    			
	    			for (j <- 0 until targets.length) {
	    				val gt = new GhostTarget
	    				gt.id = "DECOY_"+i+"_"+targets(j).id
	    				gt.peptideRef = decoyPepRef
	    				gt.q1 = dq1 + (targets(j).q1 - q1)
	    				gt.q1z = q1z
	    				gt.rtStart = targets(j).rtStart
	    				gt.rtEnd = targets(j).rtEnd
	    				gt.intensity = targets(j).intensity
	    				out += gt
	    			}
	    			
	    			val decoyQ3s = getQ3s(ions, decoyP)
	    			println("%10.3f %25s | %s".format(dq1, decoyP.toString, decoyQ3s.map(x => "%7.2f".format(x)).mkString(" ")))
	    			for (j <- 0 until decoyQ3s.length) {
	    				val gt = new GhostTransition
	    				gt.id = "DECOY_"+i+"_"+trans(j).id
	    				gt.peptideRef = decoyPepRef
	    				gt.q1 = dq1
	    				gt.q3 = decoyQ3s(j)
	    				gt.ions = ArrayBuffer("" + ions(j).molecule.fragmentType + ions(j).molecule.ordinal)
	    				gt.ce = trans(j).ce
	    				gt.rtStart = trans(j).rtStart
	    				gt.rtEnd = trans(j).rtEnd
	    				gt.intensity = trans(j).intensity
	    				out += gt
	    			}
	    		}
			}
		}
		
		
		
		def toGhostPeptide(id:String, p:Peptide, inPep:GhostPeptide):GhostPeptide = {
			var gp = new GhostPeptide
			gp.id = id
			gp.sequence = p.toString
			gp.proteins += "decoy"
			gp.tramlPeptide = inPep.tramlPeptide
			return gp
		}
		
		
		
		def getQ3s(ions:Seq[Ion[PeptideFragment]], p:Peptide):Seq[Double] = {
			var fs = p.getFragments(Array(EPeptideFragment.y, EPeptideFragment.b))
			var unused = new Queue[PeptideFragment]
			for (pf <- fs.filter(x => !ions.contains(x) && x.ordinal > 4)) unused.enqueue(pf)
			return ions.map(i => {
				Ion.mz(
					fs.find(_.same(i.molecule)).getOrElse(unused.dequeue), 
					i.numExtraProtons, 
					i.numExtraElectrons
				)
			})
		}
		
		
		def getDecoyComp(c:GhostCompound):GhostCompound = {
			val x = new GhostCompound
			x.mass = c.mass + 
				(Random.nextInt(6) - 2) * Element.C.monoisotopicWeight + 
				(Random.nextInt(12) - 5) * Element.H.monoisotopicWeight + 
				(Random.nextInt(8) - 3) * Element.O.monoisotopicWeight
			x.preferredCharges = c.preferredCharges
			x.label = c.label
			x.labelGroup = c.labelGroup
			if (x.mass == c.mass) getDecoyComp(c)
			else x
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