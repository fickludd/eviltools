package se.lth.immun

import se.lth.immun.xlink.XLink
import se.lth.immun.xlink.KojakXLink
import se.lth.immun.chem._
import se.lth.immun.unimod.UniMod

object PeptideParser {

	trait PepParseResult
	case class UniModPeptide(p:Peptide) extends PepParseResult
	case class XLinkPeptide(xl:XLink) extends PepParseResult
	case class Unparsable(seq:String) extends PepParseResult
	
	def parseSequence(seq:String):PepParseResult = {
		try {
			return UniModPeptide(UniMod.parseUniModSequence(seq))
		} catch {
			case e:Throwable =>
				e.printStackTrace
				return XLink.fromString(seq, UniMod.parseUniModSequence, KojakXLink.DSS_CARB_CONF) match {
					case Left(xl) => XLinkPeptide(xl)
					case Right(msg) => Unparsable(seq)
				}
				 
		}
	}
}