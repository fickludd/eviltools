package se.lth.immun

import se.jt.CLIApp
import java.util.Properties

import se.lth.immun.protocol.MsFragmentationFile

object Fraggle extends CLIApp {
	
	var properties = new Properties
	properties.load(this.getClass.getResourceAsStream("/pom.properties"))
	val name 		= properties.getProperty("pom.artifactId")
	val version 	= properties.getProperty("pom.version")
	
	val commands = Map(
			"interpret" -> Interpret,
			"combine" -> Combine,
			"export" -> Export)
		
	def main(args:Array[String]):Unit = {
		
		if (args.isEmpty)
			printCommandsAndExit
		
		commands.get(args.head) match {
			case Some(cmd) =>
				try {
					cmd.execute(name, version, args.head, args.tail)
				} catch {
					case e:Exception =>
						println(e)
						System.exit(1)
				}
			case None =>
				println("Unknown command '%s'".format(args.head))
				printCommandsAndExit
		}
	}
	
	
	def printCommandsAndExit = {
		println("usage: java -jar %s-%s.jar COMMAND".format(name, version))
		println("\nAvailable commands:")
		println(commands.map(x => "  %-20s %s".format(x._1, x._2.desc)).mkString("\n"))
		System.exit(1)
	}
}