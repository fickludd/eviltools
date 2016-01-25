package se.lth.immun

import java.io.File
import scala.collection.mutable.ArrayBuffer

import se.jt.Params
import se.jt.CLIApp

import se.lth.immun.protocol.AAMolecule
import se.lth.immun.protocol.MsFragmentationFile
import FraggleIntermediary._

object Combine extends Command with CLIApp {

	class CombineParams extends Params {
		import Params._
		
		val fragmentFile = ReqString("Fragment files to combine")
		
		val fdr				= 1.0			## "The FDR cutoff to apply on loaded observations"
		val mergeMethod 	= "average"		## "Method to use for observation merging (average (default), median or best)"
		
		val outDir			= ""			## "output directory (by default same as input mzML)"
		val outName			= "combined"	## "basename for output files (by default 'combined')"
		val verbose 		= false			## "set to enable a lot of output"
		val writeTsv		= false		## "set to write tsv instead of fragment binary file"
		val tsvFragN		= -1		## "number of most intense fragments to output upon writeTsv (-1 means all are written)"
		
		
		def outFile = out(outName + ".fragments.bin")
		def outTsv = out(outName + ".fragments.tsv")
		
		def out(name:String) =
			if (outDir.value != "")
				new File(outDir, name)
			else new File(name)
		
		def fragmentFiles =
			fragmentFile.value.split(" ").map(path => new File(path))
	}
	
	
	val desc = "Combine multiple fragment binary files into one consensus file"
	val params = new CombineParams
	
	def execute(name:String, version:String, command:String, args:Array[String]) = {
		
		failOnError(parseArgs(name, version, args, params, Nil, Some("fragmentFile")))
		
		printHeader(command)
		
		status("loading fragment binary files...")
		val orderedBuilders = loadFragmentBins(params.fragmentFiles)
		val merge =
			params.mergeMethod.value match {
				case "average" =>
					AverageMerger.merge _
				case "median" =>
					MedianMerger.merge _
				case "best" =>
					BestMerger.merge _
			}
		
		status("merging fragment observations...")
		val aaMolecules = orderedBuilders.map(merge)
		
		if (params.writeTsv) {
			status("writing output tsv...")
			FragmentTsv.write(params.outTsv, aaMolecules, params.tsvFragN)
		} else { 
			status("writing output binary...")
			MsFragmentationFile.write(params.outFile, aaMolecules, params.verbose)
		}
		status("done")
	}
	
	
	def loadFragmentBins(files:Seq[File]):Seq[AAMoleculeBuilder] = {
		var consensus:Seq[AAMoleculeBuilder] = Nil
		for (f <- files) {
			val aaMolecules = MsFragmentationFile.read(f, params.verbose)
			consensus = growAAMolBuilders(consensus, aaMolecules)
		}
		consensus
	}
	
	
	def growAAMolBuilders(
			sortedOld:Seq[AAMoleculeBuilder], 
			newAAMols:Seq[AAMolecule]
	):Seq[AAMoleculeBuilder] = {
		val sortedNew = newAAMols.sortBy(_.sequence)
		
		val merged = new ArrayBuffer[AAMoleculeBuilder]
		var i = 0
		var j = 0
		def isLast(sequence:String) =
			merged.nonEmpty && merged.last.sequence == sequence
		
		while (i < sortedOld.length && j < sortedNew.length) {
			val oSeq = sortedOld(i).sequence
			val nSeq = sortedNew(j).sequence
			if (isLast(nSeq)) {
				merged.last.append(sortedNew(j))
				j += 1
			} else if (oSeq == nSeq) {
				merged += sortedOld(i).append(sortedNew(j))
				i += 1
				j += 1
			} else if (oSeq < nSeq) {
				merged += sortedOld(i)
				i += 1
			} else {
				merged += AAMoleculeBuilder(sortedNew(j))
				j += 1
			}
		}
		
		while (i < sortedOld.length) {
			merged += sortedOld(i)
			i += 1
		}
		
		while (j < sortedNew.length) {
			if (merged.nonEmpty && merged.last.sequence == sortedNew(j).sequence)
				merged.last.append(sortedNew(j))
			else
				merged += AAMoleculeBuilder(sortedNew(j))
			j += 1
		}
		
		merged
	}
}