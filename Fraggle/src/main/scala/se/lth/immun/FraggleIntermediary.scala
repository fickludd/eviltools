package se.lth.immun

import scala.collection.mutable.ArrayBuffer

import se.lth.immun.protocol.FragmentAnnotation
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentationType
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType

import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.Observation

object FraggleIntermediary {

	case class RawAAMolecule(
			val sequence:String,
			val mass:Double,
			val observations:Seq[RawObservation])
			
	case class RawObservation(
		val fragmentationType:FragmentationType,
		val z:Int,
		val ce:Double,
		val precursorMz:Double,
		val precursorIntensity:Double,
		val rt:Double,
		val fragBaseIntensity:Double,
		val qValue:Double,
		val percentAnnotatedOfMS2tic:Double,
		val n:Int,
		val fragments:Seq[FragmentAnnotation])
	
		
		
		
		
		
		
	object AAMoleculeBuilder {
		def apply(x:AAMolecule) = {
			val b = new AAMoleculeBuilder(x.sequence, x.mass)
			for (obs <- x.observations) 
				b.observations += ObservationBuilder(obs)
			b
		}
	}
		
	class AAMoleculeBuilder(
			val sequence:String,
			val mass:Double
	) {
		val observations = new ArrayBuffer[ObservationBuilder]
		
		def append(aaMol:AAMolecule):AAMoleculeBuilder = {
			for (obs <- aaMol.observations) 
				observations.find(_.sameAs(obs)) match {
					case Some(builder) =>
						builder.append(obs)
					case None =>
						observations += ObservationBuilder(obs)
				}
			this
		}
	}
	
	object ObservationBuilder {
		def apply(obs:Observation) = {
			val b = new ObservationBuilder(
					obs.fragmentationType, 
					obs.z, 
					obs.ce
				)
			b.append(obs)
			b
		}
	}
	
	class ObservationBuilder(
		val fragmentationType:FragmentationType,
		val z:Int,
		val ce:Double
	) {
		val xs = new ArrayBuffer[Observation]
		
		def sameAs(obs:Observation) =
			fragmentationType == obs.fragmentationType &&
			z == obs.z &&
			ce == obs.ce
			
		def append(obs:Observation) = 
			xs += obs
	}
		
	case class FragKey(z:Int, fragmentType:FragmentType, i:Int, j:Int)
}