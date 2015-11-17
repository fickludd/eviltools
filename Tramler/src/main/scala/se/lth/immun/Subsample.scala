package se.lth.immun

import se.lth.immun.traml.clear._
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
	
	trait SubSampleMode
	case class Fraction(f:Double) extends SubSampleMode
	case class N(n:Int) extends SubSampleMode
	
	class SubsampleInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		var mode:SubSampleMode = Fraction(1)
		var seed:Option[Long] = None
		
		for ((k,v) <- params)
			k match {
				case "fraction" => mode = Fraction(v.toDouble)
				case "n" 		=> mode = N(v.toInt)
				case "seed" 	=> seed = Some(v.toLong)
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			val out = new ClearTraML
				
			out.compounds ++= subsample(in.compounds, mode, seed)
			out.proteins ++= out.compounds.flatMap(_.proteins).distinct
					
			return out
		}
	}
	
	
	def subsample[T](xs:Seq[T], mode:SubSampleMode, seed:Option[Long]) = {
		val n = 
			mode match {
				case Fraction(f) => (xs.length * f).toInt
				case N(n) => n
			}
		val r =
			seed match {
				case Some(s) => new Random(seed.get)
				case None => new Random
			}
		if (n > xs.size)
			monteCarlo(r)(xs, n)
		else
			sample(r)(xs, n)
	}	
		
	def monteCarlo[T](r:Random)(xs:Iterable[T], n:Int):Seq[T] = {
		if (n <= 0 || xs.size <= 0) return Nil
		val pool = xs.toSeq
		for (i <- 0 until n) yield 
			pool(r.nextInt(pool.length))
	}
	
	def sample[T](r:Random)(xs:Iterable[T], n:Int):Seq[T] = {
		if (n <= 0 || xs.size <= 0) return Nil
		r.shuffle(xs.toSeq).take(n)
	}
}