package se.lth.immun

import se.lth.immun.traml.ghost._
import se.lth.immun.chem._

import scala.util.Random
import scala.collection.mutable.HashMap

object Subsample extends TramlOperation.Generator(
		"subsample",
		"""  Subsamples peptides and compounds from the TraML.
    fraction     rational number specifying what percentage of peps/comps that should be kept.
    n            integer specifying in absolute number how many peps and how many comps that should be kept.
    seed         the starting point of the random number generator. Specify to get reproducible subsampling."""
) {
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) = 
		new SubsampleInstance(params, mods)
	
	class SubsampleInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var fraction = -1.0
		var n = -1
		var seed:Option[Long] = None
		
		for ((k,v) <- params)
			k match {
				case "fraction" => fraction = v.toDouble
				case "n" 		=> n = v.toInt
				case "seed" 	=> seed = Some(v.toLong)
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		if (fraction < 0 && n < 0)
			throw new IllegalArgumentException("Size of subfraction not given... need either parameter 'n' or 'fraction'")
		if (fraction >= 0 && n >= 0)
			throw new IllegalArgumentException("Cannot parse both parameter 'n' and 'fraction'. Pick one.")
		if (fraction > 1.0)
			throw new IllegalArgumentException("Monto carlo sampling not supported yet")
		
		
		def operate(in:GhostTraML, params:TramlerParams):GhostTraML = {
			
			val out = new GhostTraML
			val nCompound = if (n > 0) n else (in.compounds.size * fraction).toInt
			val compounds:Map[String, GhostCompound] =
				if (nCompound > in.compounds.size)
					monteCarlo(in.compounds, nCompound)
				else
					sample(in.compounds, nCompound)
			
			val nPeptide = if (n > 0) n else (in.peptides.size * fraction).toInt
			val peptides:Map[String, GhostPeptide] =
				if (nPeptide > in.peptides.size)
					monteCarlo(in.peptides, nPeptide)
				else
					sample(in.peptides, nPeptide)
			
			out.compounds ++= compounds
			out.peptides ++= peptides
			
			val protRefs = peptides.values.flatMap(_.proteins).toSet
			for (pr <- protRefs)
				out.proteins += pr -> in.proteins(pr)
			
			for (t <- in.includes) {
				if (compounds.contains(t.compoundRef) || peptides.contains(t.peptideRef))
					out += t
			}
			
			for (t <- in.transitions) {
				if (compounds.contains(t.compoundRef) || peptides.contains(t.peptideRef))
					out += t
			}
					
			return out
		}
		
		
		def monteCarlo[T, U](m:HashMap[T, U], n:Int):Map[T, U] = {
			if (n <= 0 || m.size <= 0) return Nil.toMap
			val pool = m.toSeq
			if (seed.isDefined) Random.setSeed(seed.get)
			(for (i <- 0 until n) yield 
					pool(Random.nextInt(pool.length))
			).toMap
		}
		
		def sample[T, U](m:HashMap[T, U], n:Int):Map[T, U] = {
			if (n <= 0 || m.size <= 0) return Nil.toMap
			if (seed.isDefined) Random.setSeed(seed.get)
			Random.shuffle(m.toSeq).take(n).toMap
		}
	}
}