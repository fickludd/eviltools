package se.lth.immun

trait Command {
	
	val t0 = System.currentTimeMillis
	
	def execute(name:String, version:String, command:String, args:Array[String])
	def desc:String
	
	def printHeader(name:String) = {
		println
		println("  "+name)
		println("  "+name.map(_ => "=").mkString)
		println
	}
	
	def timeStamp =
		"[%9.2fs] ".format((System.currentTimeMillis - t0) / 1000.0)
	
	def status(msg:String) = 
		println(timeStamp + msg)
}