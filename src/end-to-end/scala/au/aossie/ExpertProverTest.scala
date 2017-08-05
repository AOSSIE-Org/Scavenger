package au.aossie

import ammonite.ops.pwd
import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.prover.ExpertProver

/**
  * Created by vlad107 on 7/31/17.
  */
object ExpertProverTest {
  def main(args: Array[String]): Unit = {
    val tp = new TPTPCNFParser(pwd / 'tptp640).parse(pwd / 'tptp640 / 'Problems / 'NUM / s"NUM002-1.p")
    println(tp)
    new ExpertProver(4).prove(tp)
  }
}
