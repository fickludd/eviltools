package se.lth.immun

import java.io.File

import se.jt.Params
import se.jt.CLIApp

import se.lth.immun.protocol.MsFragmentationFile

object Export extends Command with CLIApp {

	class ExportParams extends Params {
		import Params._
		
		val fragmentFile = ReqString("The fragment file to export")
		
		val mode = "fragment"		## "fragment (default) or observation"
		//val maxTransitions = 6	## "The maximal number of transitions to include"
		//val minTransitions = 6	## "Exclude peptide ions with less than this number of transitions"
		
		val outDir			= ""		## "output directory (by default same as input mzML)"
		val outName			= ""		## "basename for output files (by default 'combined')"
		val verbose 		= false		## "set to enable a lot of output"
		val tsvFragN		= -1		## "number of most intense fragments to output upon writeTsv (-1 means all are written)"
		
		def outBase = {
			val fragsFile = new File(fragmentFile)
			val dir = 
				if (outDir.value != "") outDir.value
				else fragsFile.getParent
			val name =
				if (outName.value != "") outName.value
				else stripExts(fragsFile.getName)
			(dir, name)
		}
		
		def outExt(ext:String) = { 
			val (dir, name) = outBase
			new File(dir, name + ext)
		}
		
		def stripExt(path:String, ext:String) =
			if (path.toLowerCase.endsWith(ext.toLowerCase))
				path.dropRight(ext.length)
			else path
		
		def stripExts(path:String) =
			stripExt(path, ".fragments.bin")
	}
	
	val desc = "Export fragment library to tsv or traml, applying some rules"
	val params = new ExportParams
	
	def execute(name:String, version:String, command:String, args:Array[String]) = {
		
		failOnError(parseArgs(name, version, args, params, List("fragmentFile"), None))
		
		printHeader(command)
		
		status("reading fragment file...")
		val aaMolecules = MsFragmentationFile.read(new File(params.fragmentFile), params.verbose)
		
		val f = params.outExt(".%ss.tsv".format(params.mode.value))
		params.mode.value match {
			case "fragment" =>
				status("writing fragment tsv to '%s'...".format(f))
				FragmentTsv.write(f, aaMolecules, params.tsvFragN)
				
			case "observation" =>
				status("writing observation tsv to '%s'...".format(f))
				ObservationTsv.write(f, aaMolecules)
				
			case x =>
				throw new Exception("Unknown export mode '%s'".format(x))
		}
		
	}
}