package se.lth.immun

object Range {

	case class DRange(low:Double, high:Double) {
		def has(d:Double) = d >= low && d <= high
	}
	case class IRange(low:Int, high:Int) {
		def has(i:Int) = i >= low && i <= high
	}
	
	def toDoubleRange(str:String) = {
		val LH = str.split(":", 2)
		DRange(LH(0).toDouble, LH(1).toDouble)
	}
	
	def toIntRange(str:String) = {
		val LH = str.split(":", 2)
		IRange(LH(0).toInt, LH(1).toInt)
	}
}