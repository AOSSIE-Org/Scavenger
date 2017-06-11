package au.aossie

import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.prover.EPCR
import ammonite.ops._

/**
  * Created by vlad107 on 6/10/17.
  */
object EPCRTest {
  def main(args: Array[String]): Unit = {
    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'SYN / s"SYN645-1.p")
    println(tp)
    println(EPCR.prove(tp))
  }
}
