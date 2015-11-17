package se.lth.immun

import se.lth.immun.traml.clear._
import se.lth.immun.chem._

import scala.util.Random

object Distort extends TramlOperation.Generator(
		"distort",
		""" Distort traml by removing channels or changing expected intensities
    nMs1    int     remove n ms1 channels from each assay
    nMs2    int     remove n ms2 channels from each assay
    noise   float   randomly add or remove x = noise * exp.intensity from all intensities"""
) {
	
	def makeInstance(params:Seq[(String, String)], mods:Seq[IModifier]) =
		new DistortInstance(params, mods)
	
	class DistortInstance(
			params:Seq[(String, String)], 
			mods:Seq[IModifier]
	) extends TramlOperation.Instance(opString, params) {
		
		var noise = 0.0
		var nMs1 = 0
		var nMs2 = 0
		var seed:Option[Long] = None
		
		for ((k,v) <- params)
			k match {
				case "noise" 	=> noise = v.toDouble
				case "nMs1"		=> nMs1 = v.toInt
				case "nMs2"		=> nMs2 = v.toInt
				case "seed" 	=> seed = Some(v.toLong)
				case x =>
					throw new IllegalArgumentException("Unknown param '"+x+"'")
			}
		
		import Clear._
		
		def distort(r:Random, noises:Array[Double])(c:Channel) = 
			Channel(
				c.mz, 
				c.z, 
				c.id,
				c.msLevel,
				c.expIntensity.map(_ * noises(r.nextInt(2)))
			)
		
		def operate(in:ClearTraML, params:TramlerParams):ClearTraML = {
			
			val out = new ClearTraML
			out.proteins ++= in.proteins
			
			val noises = Array(1/(1+noise), 1 + noise)
			val r =
				seed match {
					case Some(s) => new Random(seed.get)
					case None => new Random
				}
			
			for (cp <- in.compounds) {
				val outCP = cp.metaCopy
				for (a <- cp.assays) {
					val outAssay = outCP.getAssay(a.mz, a.z, a.ce)
					outAssay.ms1Channels ++= 
						a.ms1Channels.map(distort(r, noises))
							.sortBy(_.expIntensity.getOrElse(0.0))
							.drop(nMs1)
					outAssay.ms2Channels ++= 
						a.ms2Channels.map(distort(r, noises))
							.sortBy(_.expIntensity.getOrElse(0.0))
							.drop(nMs1)
				}
				out.compounds += outCP
			}
			
			out
		}
	}

}