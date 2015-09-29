package se.lth.immun

import se.lth.immun.xlink.XLink
import se.lth.immun.chem._
import se.lth.immun.unimod.UniMod

object PeptideParser {

	trait PepParseResult
	case class UniModPeptide(p:Peptide) extends PepParseResult
	case class XLinkPeptide(xl:XLink) extends PepParseResult
	case class Unparsable(seq:String) extends PepParseResult
	
	def parseSequence(seq:String):PepParseResult = {
		try {
			return XLinkPeptide(XLink.fromString(seq, UniMod.parseUniModSequence))
		} catch {
			case e:Throwable =>
				try {
					return UniModPeptide(UniMod.parseUniModSequence(seq))
				} catch {
					case e:Throwable =>
						return Unparsable(seq)
				}
		}
	}
}