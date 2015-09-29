package se.lth.immun.esv

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import java.io.StringReader
import java.io.BufferedReader

class EsvReaderTest {
	
	val sample = 
"""# EXTENDED SEPARATED VALUES
# separator: ','
# escape: '"'
#
# COLUMN HEADERS
#: a, b
1,hey
4,"hey,man!"
"""
	
	val sample2 = 
"""# EXTENDED SEPARATED VALUES
# 	 separator: '	'
# 		escape: '''
# 	    sparse: false
#  num columns: 2
#     num rows: 2
#
# 	ORIGIN
# 		  source program: Anubis
# source program version: 1.2.0-SNAPSHOT
# 				  author: johant
#                   mail: johan.teleman@gmail.com
#
# PARAMETERS
# 				  author: johant
#                   mail: johan.teleman@gmail.com
#
##	Random babbling
##	is the best
##	on mondays!
#
# COLUMN HEADERS
#: a	b
1	hey
4	'hey,man!'
"""
	
	val sample3 = 
"""# EXTENDED SEPARATED VALUES
# 	 separator: '	'
# 		escape: '''
#     num rows: 2
#
# 	ORIGIN
# 		  source program: Anubis
# 				  author: johant
#                   mail: johan.teleman@gmail.com
#
# PARAMETERS
# 				  author: johant
#                   mail: johan.teleman@gmail.com
#
#
# COLUMN HEADERS
#: a	b	too
1	'	aklk'	
		ppap
"""
		
	def assertOpt[T] = _assertOpt[T, T](t => t) _
	
	def _assertOpt[T, S](f:(S) => T)(t:T, opt:Option[S]) = {
		assertTrue(opt.isDefined)
		assertEquals(t, f(opt.get))
	}
	
	@Test
	def nanoSample() = {
		val e = new EsvReader(new BufferedReader(new StringReader(sample)))
		assertEquals(',', e.separator)
		assertEquals('"', e.escape)
		assertEquals(2, e.headers.length)
		assertEquals("1", e.getValue("a"))
		assertEquals("hey", e.getValue("b"))
		e.readLine
		assertEquals("4", e.getValue("a"))
		assertEquals("hey,man!", e.getValue("b"))
	}
		
	
	@Test
	def fullSample() = {
		val e = new EsvReader(new BufferedReader(new StringReader(sample2)))
		assertEquals('\t', e.separator)
		assertEquals(''', e.escape)
		assertEquals(false, e.sparse)
		assertOpt(2, e.nColumns)
		assertOpt(2, e.nRows)
		
		assertOpt("Anubis", e.source)
		assertOpt("1.2.0-SNAPSHOT", e.version)
		assertOpt("johant", e.author)
		assertOpt("johan.teleman@gmail.com", e.mail)
		
		assertEquals("johant", e.getParameter("author").get)
		assertEquals("johan.teleman@gmail.com", e.getParameter("mail").get)
		
		_assertOpt[String, Seq[String]](_(0))("Random babbling", e.metaData)
		_assertOpt[String, Seq[String]](_(1))("is the best", e.metaData)
		_assertOpt[String, Seq[String]](_(2))("on mondays!", e.metaData)
		
		assertEquals(2, e.headers.length)
		assertEquals("1", e.getValue("a"))
		assertEquals("hey", e.getValue("b"))
		e.readLine
		assertEquals("4", e.getValue("a"))
		assertEquals("hey,man!", e.getValue("b"))
	}
		
	
	@Test
	def anotherSample() = {
		val e = new EsvReader(new BufferedReader(new StringReader(sample3)))
		assertEquals('\t', e.separator)
		assertEquals(''', e.escape)
		assertOpt(2, e.nRows)
		
		assertOpt("Anubis", e.source)
		assertTrue(!e.version.isDefined)
		assertOpt("johant", e.author)
		assertOpt("johan.teleman@gmail.com", e.mail)
		
		assertEquals("johant", e.getParameter("author").get)
		assertEquals("johan.teleman@gmail.com", e.getParameter("mail").get)

		assertEquals(3, e.headers.length)
		assertEquals("1", e.getValue("a"))
		assertEquals("	aklk", e.getValue("b"))
		assertEquals("", e.getValue("too"))
		e.readLine
		assertEquals("", e.getValue("a"))
		assertEquals("", e.getValue("b"))
		assertEquals("ppap", e.getValue("too"))
	}
	
}