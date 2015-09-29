package se.lth.immun

import se.lth.immun.xml.XmlReader
import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException

import java.io.File
import java.io.FileReader
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.util.Properties

import ms.numpress.MSNumpress
import se.lth.immun.mzml._
import se.lth.immun.mzml.ghost._

object PrecursorAnalyzer extends CLIApplication {
	
	
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.name")
	val version 	= properties.getProperty("pom.version")
	
	def isIMzML(f:File) 	= f.getName.toLowerCase.endsWith(".imzml")
	def toIBD(f:File)		= new File(f.getAbsolutePath().dropRight(5) + "ibd")
	
	
	
	var mzMLFile:File 	= _
	var xr:XmlReader 	= _
    var force = true	
    var ms1AGC = 3e6
    var ms1maxInjTime = 50.0
    var ms2AGC = 2e5
    var ms2maxInjTime = 50.0
	var ms1:Spectrum = _
    
	def main(args:Array[String]):Unit = {
		
		arg("MZML", s => {
			mzMLFile = new File(s)
		})
		
		opt("force", 
				"Ignore missing attributes in mzML files", 
				s => force = true)
		
		opt("ms1-AGC", 
				"Target amount of ions for ms1 spectra (default %.1e)".format(ms1AGC), 
				s => ms1AGC = s.toDouble, "X")
		
		opt("ms1-filltime", 
				"Maximum inj time for ms1 spectra (default %.1fms)".format(ms1maxInjTime), 
				s => ms1maxInjTime = s.toDouble, "X")
		
		opt("ms2-AGC", 
				"Target amount of ions for ms2 spectra (default %.1e)".format(ms2AGC), 
				s => ms2AGC = s.toDouble, "X")
		
		opt("ms2-filltime", 
				"Maximum inj time for ms2 spectra (default %.1fms)".format(ms2maxInjTime), 
				s => ms2maxInjTime = s.toDouble, "X")
		
		try {
			parseArgs(name + " "+version, args)
		} catch {
			case cae:CommandlineArgumentException => return
		}
		
		xr = new XmlReader(
				if (mzMLFile.getName.toLowerCase.endsWith(".gz"))
					new BufferedReader(new InputStreamReader(
						new GZIPInputStream(new FileInputStream(mzMLFile))))
				else
					new BufferedReader(new FileReader(mzMLFile))
			)
		xr.force = force
		readMzMLFile(xr, mzMLFile)
	}
	
	
	
	def readMzMLFile(xr:XmlReader, swathFile:File):Unit = {
		var dh = new MzMLDataHandlers(
				setupDataStructures,
				handleSpectrum,
				nc => {},
				c => {})
		
		val binaryFileChannel = 
			if (isIMzML(swathFile)) {
				val binaryFile = toIBD(swathFile)
				println("  BINARY FILE "+binaryFile)
				new FileInputStream(binaryFile).getChannel()
			} else null
		var mzML = MzML.fromFile(xr, dh, binaryFileChannel)
	}
	
	
	
	def setupDataStructures(numSpec:Int) = {
		println(Array(
					"ms1.index",
					"ms2.index",
					"prec.q1",
					"prec.intensity",
					"prec.intensity.of.tic",
					"prec.intensity.of.precs",
					"prec.proj.injTime",
					"prec.est.injTime",
					"est.injTimeSum",
					"ms2.injTime",
					"ms2.scanStartTime",
					"ms2.n.precs",
					"ms2.tic",
					"ms2.nvalues"
				).mkString("\t"))
	}
	
	
	case class PrecInfo(
			q1:Double, 
			intensity:Double,  
			ionPerMs:Double, projInjTime:Double)
	
	def handleSpectrum(rawSpec:se.lth.immun.mzml.Spectrum):Unit = {
		var gs = GhostSpectrum.fromSpectrum(rawSpec)
		
		import Ghost._
		
		if (gs.msLevel == 1) {
			ms1 = Spectrum.parse(rawSpec, gs)
		} else if (gs.msLevel == 2) {
			
			val s = Spectrum.parse(rawSpec, gs)
			
			val precInfos = 
				for (p <- s.precursors) yield {
					val intensity = ms1.area(p.low, p.high)
					
					val ionPerMs =
						if (ms1.injTime < ms1maxInjTime) (ms1AGC * intensity / ms1.tic) / ms1.injTime
						else 1
					
					val projInjTime = ms2AGC / ionPerMs
					
					PrecInfo((p.high + p.low) / 2, intensity, ionPerMs, projInjTime)
				}
			
			val precsSum = precInfos.map(_.intensity).sum
			val injTimeSum = precInfos.map(pi => math.min(pi.projInjTime, ms2maxInjTime)).sum
			
			for (pi <- precInfos) {
				println(Array(
					ms1.index,
					s.index,
					pi.q1,
					pi.intensity,
					pi.intensity / ms1.tic,
					pi.intensity / precsSum,
					pi.projInjTime,
					math.min(pi.projInjTime, ms2maxInjTime),
					injTimeSum,
					s.injTime,
					s.scanStartTime,
					s.precursors.length,
					s.tic,
					s.mzs.length
				).mkString("\t"))
			}
		}
	}
}
