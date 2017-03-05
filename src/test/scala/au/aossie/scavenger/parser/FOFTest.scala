package au.aossie.scavenger.parser

import ammonite.ops.pwd
import au.aossie.scavenger.prover.EPCR

/**
  * Created by vlad107 on 3/3/17.
  */
object FOFTest {
  def main(args: Array[String]):Unit = {
    val path = pwd / 'examples / 'problems / 'FOF / "FOF.fofp"
    val cnf = TPTPFOFParser.parse(path)
    println(cnf.clauses.mkString("\n"))
    println(EPCR.prove(cnf))
  }
}
