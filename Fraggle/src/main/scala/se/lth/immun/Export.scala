package se.lth.immun

import java.io.File

import se.jt.Params
import se.jt.CLIApp

import se.lth.immun.protocol.MsFragmentationFile

object Export extends Command with CLIApp {

	class ExportParams extends Params {
		import Params._
		
		val fragmentFile = ReqString("The fragment file to export")
		
		val outType = ""		## "Type of output (traml(default) or tsv)"
		//val maxTransitions = 6	## "The maximal number of transitions to include"
		//val minTransitions = 6	## "Exclude peptide ions with less than this number of transitions"
		
		val outDir			= ""			## "output directory (by default same as input mzML)"
		val outName			= "combined"	## "basename for output files (by default 'combined')"
		val verbose 		= false			## "set to enable a lot of output"
		
		def outTsv = 
			new File(outDir, outName + ".fragments.tsv")
	}
	
	val desc = "Export fragment library to tsv or traml, applying some rules"
	val params = new ExportParams
	
	def execute(name:String, version:String, command:String, args:Array[String]) = {
		
		failOnError(parseArgs(name, version, args, params, List("fragmentFile"), Some("fragmentFile")))
		
		printHeader(command)
		
		status("reading fragment file...")
		val aaMolecules = MsFragmentationFile.read(new File(params.fragmentFile), params.verbose)
		status("writing output tsv...")
		FragmentTsv.write(params.outTsv, aaMolecules)
	}
}