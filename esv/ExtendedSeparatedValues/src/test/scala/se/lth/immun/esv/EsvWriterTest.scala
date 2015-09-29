package se.lth.immun.esv

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import java.io.StringWriter
import java.io.BufferedWriter

class EsvWriterTest {
	
	val sample = 
"""# EXTENDED SEPARATED VALUES
#    separator: ','
#       escape: '"'
#
# COLUMN HEADERS
#: a,b
1,hey
4,"hey,man!"
"""
	
	val sample2 = 
"""# EXTENDED SEPARATED VALUES
#    separator: '	'
#       escape: '''
#  num columns: 2
#     num rows: 2
#
# ORIGIN
#         source program: Anubis
# source program version: 1.2.0-SNAPSHOT
#                 author: johant
#                   mail: johan.teleman@gmail.com
#
# PARAMETERS
# author: johant
# mail: johan.teleman@gmail.com
#
## Random babbling
## is the best
## on mondays!
#
# COLUMN HEADERS
#: a	b
1	hey
4	'hey	man!'
"""
	
	val sample3 = 
"""# EXTENDED SEPARATED VALUES
#    separator: '	'
#       escape: '''
#     num rows: 2
#
# ORIGIN
#         source program: Anubis
#                 author: johant
#                   mail: johan.teleman@gmail.com
#
# PARAMETERS
# author: johant
# mail: johan.teleman@gmail.com
#
# COLUMN HEADERS
#: a	b	too
1	hey	'	aklk'
4	'hey	man!'	ppap
"""
		
	@Test
	def nanoSample() = {
		var e = new Esv
		e.separator = ','
		e.escape = '"'
		e.headers = Array("a", "b")
		
		val sw = new StringWriter
		val bw = new BufferedWriter(sw)
		
		var ew = new EsvWriter(e, bw)
		ew.write(List(1, "hey"))
		ew.write(List(4, "hey,man!"))
		
		ew.close
		
		assertEquals(sample, sw.toString)
	}
		
	@Test
	def fullSample() = {
		var e = new Esv
		e.separator = '\t'
		e.escape = '''
		e.nColumns = Some(2)
		e.nRows = Some(2)
		
		e.source = Some("Anubis")
		e.version = Some("1.2.0-SNAPSHOT")
		e.author = Some("johant")
		e.mail = Some("johan.teleman@gmail.com")
		
		e.addParameter("author", "johant")
		e.addParameter("mail", "johan.teleman@gmail.com")
		
		e.metaData = Some(Array("Random babbling", "is the best", "on mondays!"))
		
		e.headers = Array("a", "b")
		
		
		val sw = new StringWriter
		val bw = new BufferedWriter(sw)
		
		var ew = new EsvWriter(e, bw)
		ew.write(List(1, "hey"))
		ew.write(List(4, "hey\tman!"))
		
		ew.close
		
		assertEquals(sample2, sw.toString)
	}
		
	@Test
	def mixedSample() = {
		var e = new Esv
		e.separator = '\t'
		e.escape = '''
		e.nRows = Some(2)
		
		e.source = Some("Anubis")
		e.author = Some("johant")
		e.mail = Some("johan.teleman@gmail.com")
		
		e.addParameter("author", "johant")
		e.addParameter("mail", "johan.teleman@gmail.com")
		
		e.headers = Array("a", "b", "too")
		
		
		val sw = new StringWriter
		val bw = new BufferedWriter(sw)
		
		var ew = new EsvWriter(e, bw)
		ew.write(List(1, "hey", "\taklk"))
		ew.write(List(4, "hey\tman!", "ppap"))
		
		ew.close
		
		assertEquals(sample3, sw.toString)
	}
}