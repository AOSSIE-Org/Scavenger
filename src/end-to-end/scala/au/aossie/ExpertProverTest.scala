package au.aossie

import ammonite.ops.pwd
import au.aossie.scavenger.parser.TPTPCNFParser
import au.aossie.scavenger.prover.{ExpertProver, Satisfiable, Unsatisfiable}

/**
  * Created by vlad107 on 7/31/17.
  */
object ExpertProverTest {
  def main(args: Array[String]): Unit = {
    val tp = new TPTPCNFParser(pwd / 'tptp640).parse(pwd / 'tptp640 / 'Problems / 'SYN / s"SYN040-1.p")
    println(tp)
    val problemStatus = new ExpertProver(numActors = 1, withSetOfSupport = true).prove(tp)
    problemStatus match {
      case Unsatisfiable(proof) =>
        println(proof)
      case Satisfiable(model) =>
        println(model)
    }
  }
}
