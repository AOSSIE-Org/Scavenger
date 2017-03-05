package au.aossie.scavenger.parser

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.prover.EPCR
import au.aossie.scavenger.structure.immutable.CNF

/**
  * Created by vlad107 on 3/3/17.
  */
object TPTPClausifier {
  def clausify(formula: E): CNF = {
    val cnfForm = FOFE.toProp(formula)
      .miniScope()
      .negationsIn()
      .skolemize()
      .forallOut()
//    println(cnfForm)
    cnfForm.toCNF
//    println(cnfForm)
  }
}
