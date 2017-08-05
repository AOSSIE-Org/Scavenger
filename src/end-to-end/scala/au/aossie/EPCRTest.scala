package au.aossie

import au.aossie.scavenger.parser.{TPTPCNFParser, TPTPFOFParser}
import au.aossie.scavenger.prover.EPCR
import ammonite.ops._

/**
  * Created by vlad107 on 6/10/17.
  */
object EPCRTest {
  def main(args: Array[String]): Unit = {
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'NUM / s"NUM002-1.p")  // simple number theory problem
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'PUZ / s"PUZ010-1.p") // einstain problem
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'GRP / s"GRP124-1.004.p") //
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'MSC / s"MSC007-1.008.p") //
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'LCL / s"LCL003-1.p") //
//    val tp = TPTPCNFParser.parse(pwd / 'tptp640 / 'Problems / 'SWV / s"SWV002-1.p", Some(pwd / 'tptp640)) //
    val tp = new TPTPFOFParser(pwd / 'tptp640).parse(pwd / 'tptp640 / 'Problems / 'LCL / s"LCL640+1.005.p") //
    println(tp)
    println(EPCR.prove(tp))
  }
}
