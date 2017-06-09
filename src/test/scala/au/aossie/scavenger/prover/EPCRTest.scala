package au.aossie.scavenger.prover

import ammonite.ops.pwd
import au.aossie.scavenger.parser.TPTPFOFParser

/**
  * Created by vlad107 on 6/7/17.
  */
class EPCRTest {
  def main(args: Array[String]):Unit = {
    val path = pwd / 'examples / 'problems / 'FOF / "FOF.fofp"
    val cnf = TPTPFOFParser.parse(path)
    //    println(cnf.clauses.mkString("\n"))
    //    println(EPCR.prove(cnf))
  }
}
