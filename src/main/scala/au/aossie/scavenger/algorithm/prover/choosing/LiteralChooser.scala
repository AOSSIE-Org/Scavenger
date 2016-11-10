package au.aossie.scavenger.algorithm.prover.choosing

import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal
import au.aossie.scavenger.algorithm.prover.structure.mutable.CNF

/**
  * Represents general way to choose next literal.
  *
  * @author Daniyar Itegulov
  */
trait LiteralChooser {
  /**
    * Tries to choose best literal to make decision on.
    *
    * @param cnf which has some undecided literals
    * @return None, if all literals have decisions
    *         Some(literal), if literal should be decided
    */
  def chooseLiteral(cnf: CNF): Option[Literal]
}
