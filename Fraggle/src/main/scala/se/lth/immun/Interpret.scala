package se.lth.immun

import se.jt.Params
import se.jt.CLIApp
import java.io.File

import MSGF._
import FraggleIntermediary._

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File
import java.io.FileReader
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._
import scala.util.{Either, Left, Right}

import se.lth.immun.xml.XmlReader
import se.lth.immun.mzml.MzML
import se.lth.immun.mzml.Spectrum
import se.lth.immun.mzml.SelectedIon
import se.lth.immun.mzml.MzMLDataHandlers
import se.lth.immun.mzml.ghost.GhostSpectrum

import se.lth.immun.protocol._
import se.lth.immun.protocol.MSFragmentationProtocol.FragmentType
import se.lth.immun.unimod.UniMod
import se.lth.immun.chem.Ion
import se.lth.immun.chem.Peptide
import se.lth.immun.chem.Constants
import se.lth.immun.chem.PeptideUtil
import se.lth.immun.chem.PeptideFragment
import se.lth.immun.chem.EPeptideFragment




object Interpret extends Command with CLIApp {
	val MS_LEVEL_ACC = "MS:1000511"
	val SELECTED_ION_MZ_ACC = "MS:1000744"
	val CHARGE_STATE_ACC = "MS:1000041"
	val PEAK_INTENSITY_ACC = "MS:1000042"
	val COLLISION_ENERGY_ACC = "MS:1000045"
	val HCD_ACC = "MS:1000422"
	val CID_ACC = "MS:1000133"
	val ETD_ACC = "MS:1000598"
	
	case class InternalIon(first:Int, last:Int, mz:Double, z:Int)
	
	class InterpretParams extends Params {
		import Params._
		
		val mzML 		= ReqString("mzML to create fraggle file from")
		val identTsv 	= ReqString("ident tsv to create fraggle file from")
		val irt		 	= ReqString("irt peptides to create rt -> irt map")
		
		val fragMatchTol = 10.0 	## "Tolerance for fragment interpretation (ppm)"
		val fragMaxCharge = 2 		## "Maximal fragment charge to consider"
		val minInternalLength = 2	## "Min length of internal ions to consider"
		val maxInternalLength = 6	## "Max length of internal ions to consider"
		val fdr = 1.0				## "The FDR cutoff to apply on loaded identifications"
		
		val outDir			= ""		## "output directory (by default same as input mzML)"
		val outName			= ""		## "basename for output files (by default same as input mzML)"
		val verbose 		= false		## "set to enable a lot of output"
		val writeTsv		= false		## "set to write tsv instead of fragment binary file"
		
		def outBase = {
			val mzMLFile = new File(mzML)
			val dir = 
				if (outDir.value != "") outDir.value
				else mzMLFile.getParent
			val name =
				if (outName.value != "") outName.value
				else stripExts(mzMLFile.getName)
			(dir, name) 
		}
		
		def outFile = { 
			val (dir, name) = outBase
			new File(dir, name + ".fragments.bin")
		}
		
		def outTsv = { 
			val (dir, name) = outBase
			new File(dir, name + ".fragments.tsv")
		}
		
		def stripExt(path:String, ext:String) =
			if (path.toLowerCase.endsWith(ext.toLowerCase))
				path.dropRight(ext.length)
			else path
		
		def stripExts(path:String) =
			stripExt(stripExt(path, ".gz"), ".mzML")
	}
	
	
	

	val desc = "Interpret ms2 spectra in mzML using MSGF+ search results and save in fragment file"
	val params = new InterpretParams
	
	def execute(name:String, version:String, command:String, args:Array[String]) = {
		
		failOnError(parseArgs(name, version, args, params, List("mzML", "identTsv", "irt"), None))
		
		printHeader(command)
		
		status("reading identifications...")
		val byScan = MSGF.fromFile(new File(params.identTsv), params.fdr).sortBy(_.scanNum).toArray
		status("interpreting mzML...")
		val rawAAMolecules = parseMzML(new File(params.mzML), byScan)
		
		status("reading iRT peptides...")
		val irtPeptides = IRT.readPeptideTsv(new File(params.irt))
		status("extracting iRT peptide identifications...")
		val irtDataPoints = IRT.findDataPoints(irtPeptides, rawAAMolecules)
		status("building iRT map...")
		val irtMap = IRT.createMap(irtDataPoints)
		
		println("IRT MAP:")
		println(irtMap)
		
		status("calculating iRTs...")
		val aaMolecules:Seq[AAMolecule] = rawAAMolecules.map(irtMap)
		
		if (params.writeTsv) {
			status("writing output tsv...")
			FragmentTsv.write(params.outTsv, aaMolecules)
		} else {
			status("writing output binary...")
			MsFragmentationFile.write(params.outFile, aaMolecules, params.verbose)
		}
		status("done")
	}
	
	
	def parseMzML(f:File, byScan:Array[MSGFid]) = {
		
		val aaMolecules = new ArrayBuffer[RawAAMolecule]
		val dh = new MzMLDataHandlers(
			n => {},
			handleSpectrum(aaMolecules, byScan),
			n => {},
			c => {})
		
		MzML.fromFile(getReader(f), dh, null)
		aaMolecules
	}
	
	
	var idIndex = 0
	val scanNumRE = """scan=(\d+)""".r.unanchored
	def handleSpectrum(
			aaMolecules:ArrayBuffer[RawAAMolecule],
			byScan:Array[MSGFid]
	)(
			s:Spectrum
	) = {
		
		val msLevel = s.cvParams.find(_.accession == MS_LEVEL_ACC).map(_.value.get.toInt)
		val scanNum = s.id match {
			case scanNumRE(num) => num.toInt
			case _ => 
				throw new Exception("Couldn't parse scan number from spectrum title '%s'".format(s.id))
		}
		
		lazy val gs = GhostSpectrum.fromSpectrum(s)
		while (idIndex < byScan.length && byScan(idIndex).scanNum <= scanNum) {
			
			if (byScan(idIndex).scanNum == scanNum) {
				msLevel match {
					case Some(x) =>
						if (x != 2)
							throw new Exception("[SCAN_NUM:%d] Will only match ids to spectra with ms level 2, got %d".format(scanNum, x))
					case None =>
						throw new Exception("[SCAN_NUM:%d] No msLevel annotation found in spectrum!".format(scanNum))
				}
				
				// check centroiding!!
				
				try {
					aaMolecules += interpret(gs, byScan(idIndex))
				} catch {
					case e:Exception =>
						println("[SPEC:%d %s] Failed interpretation".format(scanNum, s.id))
						e.printStackTrace
				}
			}
			
			idIndex += 1
		}
	}
	
	
	def interpret(gs:GhostSpectrum, id:MSGFid):RawAAMolecule = {
		
		import EPeptideFragment._
		
		val pep = UniMod.parseUniModSequence(id.pepSequence)
		val possFragIons:Seq[(Double, Either[Ion[PeptideFragment], InternalIon])] = 
			PeptideUtil.possibleIons(pep, Array(a,b,c,x,y,z), params.fragMaxCharge.value).map(x => (x.mz, Left(x))) ++
			ionize(genInternalFragments(pep, params.minInternalLength, params.maxInternalLength), params.fragMaxCharge).map(x => (x.mz, Right(x)))
		
		val sortedFragIons = possFragIons.sortBy(_._1)	
		
		val fragIons = new ArrayBuffer[FragmentAnnotation]
		var i = 0
		var j = 0
		while (i < gs.mzs.length && j < sortedFragIons.length) {
			val (fmz, fdesc) = sortedFragIons(j)
			val mzDiff = gs.mzs(i) - fmz
			if (math.abs(mzDiff) / fmz * 1e6 < params.fragMatchTol) {
				val fragIon = 
					fdesc match {
						case Left(fi) =>
							SimpleFragment(
								BaseFragment(
									gs.intensities(i), 
									fi.numExtraProtons - fi.numExtraElectrons, 
									Some(fmz),
									None,
									1
								),
								toFragType(fi.molecule.fragmentType),
								fi.molecule.ordinal
							)
						case Right(ii) => 
							InternalFragment(
								BaseFragment(
									gs.intensities(i), 
									ii.z, 
									Some(ii.mz),
									None,
									1
								),
								ii.first, 
								ii.last
							)
					}
				fragIons += fragIon
				i += 1
				j += 1
			} else if (mzDiff < 0) {
				
				i += 1
			} else {
				
				j += 1
			}
		}
		
		val (ft, ce) = parseFragmentationType(gs.spectrum)
		val fragBaseIntensity = 
			if (fragIons.nonEmpty) fragIons.map(_.base.intensity).max
			else 1.0
		
		RawAAMolecule(
				id.pepSequence, 
				pep.monoisotopicMass, 
				Array(RawObservation(
						ft,
						id.z,
						ce,
						id.precursorMz,
						parsePrecursorIntensity(gs.spectrum, id.precursorMz, id.z).getOrElse(0.0),
						gs.scanStartTime,
						fragBaseIntensity,
						id.qValue,
						fragIons.map(_.base.intensity).sum / gs.intensities.sum,
						1,
						normalize(fragIons, fragBaseIntensity)
					))
			)
	}
	
	
	def normalize(fragIons:Seq[FragmentAnnotation], base:Double) =
		for (fi <- fragIons) yield
			fi match {
				case SimpleFragment(BaseFragment(int, z, mz, intStd, n), ftype, ordinal) =>
					SimpleFragment(BaseFragment(int / base, z, mz, intStd, n), ftype, ordinal)
				case XLinkFragment(BaseFragment(int, z, mz, intStd, n), ftype, ordinal, pep) =>
					XLinkFragment(BaseFragment(int / base, z, mz, intStd, n), ftype, ordinal, pep)
				case InternalFragment(BaseFragment(int, z, mz, intStd, n), first, last) =>
					InternalFragment(BaseFragment(int / base, z, mz, intStd, n), first, last)
			}
	
	
	def genInternalFragments(pep:Peptide, minLength:Int, maxLength:Int) = {
		val aas = pep.aminoAcids
		val aaMasses = aas.map(_.monoisotopicMass)
		for {
			i <- 1 until aas.length - minLength
			j <- i+minLength until math.min(aas.length, i + maxLength)
		} yield (i, j, aaMasses.slice(i, j).sum)
	}
	
	
	def ionize(frags:Seq[(Int, Int, Double)], maxCharge:Int):Seq[InternalIon] = {
		for {
			z <- 1 to maxCharge 
			(i, j, m) <- frags
		} yield InternalIon(i, j-1, m / z + Constants.PROTON_WEIGHT, z)
	}
	
	
	def selectedIon2Mz(si:SelectedIon):Option[(Double, Int, Double)] = 
		 for {
			 mzCV 	<- si.cvParams.find(_.accession == SELECTED_ION_MZ_ACC)
			 zCV 	<- si.cvParams.find(_.accession == CHARGE_STATE_ACC)
			 intCV 	<- si.cvParams.find(_.accession == PEAK_INTENSITY_ACC)
		 } yield (mzCV.value.get.toDouble, zCV.value.get.toInt, intCV.value.get.toDouble)
	
	
	def parsePrecursorIntensity(s:Spectrum, idMz:Double, idz:Int) = 
		(for {
			p <- s.precursors
			si <- p.selectedIons
			(mz, z, int) <- selectedIon2Mz(si)
			if idMz == mz && idz == z
		} yield int).headOption
	
	
	def toFragType(t:EPeptideFragment):FragmentType = 
		t match {
			case EPeptideFragment.a => FragmentType.A
			case EPeptideFragment.b => FragmentType.B
			case EPeptideFragment.c => FragmentType.C
			case EPeptideFragment.x => FragmentType.X
			case EPeptideFragment.y => FragmentType.Y
			case EPeptideFragment.z => FragmentType.Z
		}
		 
	
	def parseFragmentationType(s:Spectrum):(MSFragmentationProtocol.FragmentationType, Double) = {
		import MSFragmentationProtocol.FragmentationType._
		
		val a = s.precursors.head.activation
		val ft =
			if (a.cvParams.exists(_.accession == HCD_ACC)) HCD
			else if (a.cvParams.exists(_.accession == CID_ACC)) CID
			else if (a.cvParams.exists(_.accession == ETD_ACC)) ETD
			else
				throw new Exception("Unknown activation of spectrum '%s'".format(s.id))
		
		val ce =
			a.cvParams.find(_.accession == COLLISION_ENERGY_ACC) match {
				case Some(cv) => cv.value.get.toDouble
				case None => throw new Exception("No collision energy found in spectrum '%s'".format(s.id))
			}
			
		(ft, ce)
	}
	
	
	def getReader(f:File):XmlReader = {
		val name = f.getName
		if (name.toLowerCase.endsWith(".mzml.gz"))
			new XmlReader(new BufferedReader(new InputStreamReader(
							new GZIPInputStream(new FileInputStream(f)))))
		else if (name.toLowerCase.endsWith(".mzml"))
			new XmlReader(new BufferedReader(new FileReader(f)))
		else
			throw new Exception("Unknown file format '%s'".format(name))
	}
}