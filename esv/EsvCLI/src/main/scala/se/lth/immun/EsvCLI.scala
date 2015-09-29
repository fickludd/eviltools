package se.lth.immun

import se.lth.immun.app.CLIApplication
import se.lth.immun.app.CommandlineArgumentException

object EsvCLI {

	
	def printUsage = {
		println("""esv COMMAND
commands:
	join
	filter
	clean
	unique
	markup
	concat
	partition
	help
type 'esv COMMAND' for more help on that command""")
	}
	
	var cmd:Command = null
	
	def main(args:Array[String]):Unit = {
		
		if (args.isEmpty)
			return printUsage
			
		args.head match {
			case Join.str => {
				cmd = Join(args.tail)
			}
			case Filter.str => {
				cmd = Filter(args.tail)
			}
			case Clean.str => {
				cmd = Clean(args.tail)
			}
			case Unique.str => {
				cmd = Unique(args.tail)
			}
			case Markup.str => {
				cmd = Markup(args.tail)
			}
			case Concat.str => {
				cmd = Concat(args.tail)
			}
			case Partition.str => {
				cmd = Partition(args.tail)
			}
			case Count.str => {
				cmd = Count(args.tail)
			}
			case x => {
				println("Unknown command '"+x+"'")
				return printUsage
			}
		}
		
		cmd.execute
	}
}