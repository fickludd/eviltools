object fragmentRE {
  val riRE = """([abcxyz])(\d+)""".r.unanchored   //> riRE  : scala.util.matching.UnanchoredRegex = ([abcxyz])(\d+)
		val iiRE = """m([0-9]+):([0-9]+)""".r.unanchored
                                                  //> iiRE  : scala.util.matching.UnanchoredRegex = m([0-9]+):([0-9]+)
		
		val frags = Array("y1", "b5", "x2991", "m4:6", "y7/-0.001,b8/0.32")
                                                  //> frags  : Array[String] = Array(y1, b5, x2991, m4:6, y7/-0.001,b8/0.32)
		for (f <- frags) {
			f match {
				case riRE(f, o) => println("got riRE(%s,%d)".format(f, o.toInt))
				case iiRE(s, e) => println("got iiRE(%d,%d)".format(s.toInt, e.toInt))
			}                         //> got riRE(y,1)
                                                  //| got riRE(b,5)
                                                  //| got riRE(x,2991)
                                                  //| got iiRE(4,6)
                                                  //| got riRE(y,7)
		}
}