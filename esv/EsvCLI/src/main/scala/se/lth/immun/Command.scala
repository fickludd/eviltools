package se.lth.immun

trait Command {
	val str:String
	def apply(args:Array[String]):Command
	def execute
}