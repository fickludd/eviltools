package se.lth.immun

import java.io.File

import se.lth.immun.traml.clear._
import se.lth.immun.protocol.MsFragmentationFile
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType
import se.lth.immun.protocol.FragmentAnnotation
import se.lth.immun.protocol.SimpleFragment
import se.lth.immun.protocol.XLinkFragment
import se.lth.immun.protocol.InternalFragment
import se.lth.immun.chem._
import se.lth.immun.xlink.XLink

object FragmentBin {
	
	object FragmentMassCalculator {
		import PeptideParser._
		import FragmentType._
		
		def apply(seq:String) =
			PeptideParser.parseSequence(seq) match {
				case Unparsable(seq) =>
					new UnparsablePep(seq)
				case UniModPeptide(p) =>
					new UniModCalculator(p)
				case XLinkPeptide(xl) =>
					new XLinkCalculator(xl)
			}
		
		class UnparsablePep(seq:String) extends FragmentMassCalculator {
			def calc(fa:FragmentAnnotation) =
				throw new Exception("Cannot parse fragments for AA sequence '%s'. Sequence is not parsable!".format(seq))
		}
		
		class UniModCalculator(p:Peptide) extends FragmentMassCalculator {
			def calc(fa:FragmentAnnotation) =
				fa match {
					case SimpleFragment(base, ftype, ord) =>
						fragmentTypeMass(p, ftype, ord)
					case InternalFragment(base, first, last) =>
						sumAAs(p, first, last)
					case xlf:XLinkFragment =>
						throw new Exception("Cannot interpret xlinkFragment %s for regular peptide %s!".format(xlf, p))
				}
		}
		
		class XLinkCalculator(xl:XLink) extends FragmentMassCalculator {
			def calc(fa:FragmentAnnotation) =
				fa match {
					case XLinkFragment(base, ftype, ord, iPep) =>
						if (iPep == 1)
							fragmentTypeMass(xl.pep1, ftype, ord)
						else if (iPep == 2)
							xl.pep2 match {
								case Some(pep2) => fragmentTypeMass(pep2, ftype, ord)
								case None => 
									throw new Exception("Fragment annotated as coming from the second peptide, but there is only one peptide in '%s'".format(xl))
							}
						else
							throw new Exception("Fragment annotated as coming from peptide %d, only peptide 1 or 2 are valid values".format(iPep))
					case fa:FragmentAnnotation =>
						throw new Exception("Cannot interpret regular fragment %s on a xlinked peptide %s! Don't know which of the xlinked peptides to use".format(fa, xl))
						
				}
		}
		
		def fragmentTypeMass(p:Peptide, ftype:FragmentType, ord:Int) =
			ftype match {
				case A => sumFirstAAs(p, ord) - EPeptideFragment.A_MASS_DIFF
				case B => sumFirstAAs(p, ord)
				case C => sumFirstAAs(p, ord) + EPeptideFragment.CZ_MASS_DIFF
				case X => sumLastAAs(p, ord) + EPeptideFragment.X_MASS_DIFF
				case Y => sumLastAAs(p, ord)
				case Z => sumLastAAs(p, ord) - EPeptideFragment.CZ_MASS_DIFF
				case M => throw new Exception("SimpleFragment fragment type can't be internal (M)!")
			}
		
		def sumLastAAs(p:Peptide, n:Int) = {
			val aas = p.aminoAcids
			aas.takeRight(n).map(_.monoisotopicMass).sum + Constants.WATER_WEIGHT
		}
		
		def sumFirstAAs(p:Peptide, n:Int) = {
			val aas = p.aminoAcids
			aas.take(n).map(_.monoisotopicMass).sum
		}
		
		def sumAAs(p:Peptide, first:Int, last:Int) = {
			val aas = p.aminoAcids
			aas.slice(first, last+1).map(_.monoisotopicMass).sum
		}
	}
	
	trait FragmentMassCalculator {
		def calc(fa:FragmentAnnotation):Double
	}
	
	
	def parse(f:File):ClearTraML = {
		val aaMolecules = MsFragmentationFile.read(f, false)
		
		import PeptideParser._
		import Clear._
		
		val c = new ClearTraML
		
		for {
			aaMol <- aaMolecules
			if aaMol.observations.exists(_.fragments.nonEmpty)
		} {
			c.proteins += aaMol.protein
			val cp = new ClearPeptide(aaMol.sequence, Array(aaMol.protein))
			cp.rt = Some(ClearRetentionTime.IRT(mean(aaMol.observations.flatMap(_.iRT))))
			
			lazy val pepMass = 
				PeptideParser.parseSequence(aaMol.sequence) match {
					case UniModPeptide(pep) => pep.monoisotopicMass
					case XLinkPeptide(xl) => xl.monoisotopicMass
					case Unparsable(str) => 
						throw new Exception("Cannot parse peptide '%s' and peptide m/z not annotated. Exiting!\n%s".format(aaMol.sequence, str))
				}
			lazy val fragMassCalc = FragmentMassCalculator(aaMol.sequence)
			
			for (obs <- aaMol.observations.filter(_.fragments.nonEmpty)) {
				val a = 
					cp.getAssay(
						aaMol.mass / obs.z + Constants.PROTON_WEIGHT,
						obs.z,
						Some(obs.ce)
					)
				
				for (frag <- obs.fragments) {
					a.ms2Channels += Channel(
							frag.base.mz.getOrElse(fragMassCalc.calc(frag)), 
							frag.base.z, 
							toIonString(frag), 
							2,
							Some(frag.base.intensity)
						)
				}
			}
			
			c.compounds += cp
		}
		
		c
	}
	
	def toIonString(fa:FragmentAnnotation):String = 
		fa match {
		case SimpleFragment(base, ftype, ord) => toIonString(ftype, ord)
		case XLinkFragment(base, ftype, ord, pep) => "pep=%d %s".format(pep, toIonString(ftype, ord))
		case InternalFragment(base, first, last) => "m%d:%d".format(first, last)
	}
	
	def toIonString(ftype:FragmentType, ord:Int):String = {
		import FragmentType._
		ftype match {
			case A => "a"+ord
			case B => "b"+ord
			case C => "c"+ord
			case X => "x"+ord
			case Y => "y"+ord
			case Z => "z"+ord
			case M => throw new Exception("Cannot handle internal fragments here!")
		}
	}

	def pluses(n:Int) =
		"++++++++++++++++++++++".take(n)
		
	def mean(xs:Seq[Double]) =
		xs.sum / xs.length
}