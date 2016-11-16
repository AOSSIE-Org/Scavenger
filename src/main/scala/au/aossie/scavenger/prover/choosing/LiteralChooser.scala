package au.aossie.scavenger.prover.choosing

import au.aossie.scavenger.structure.immutable.Literal
import au.aossie.scavenger.prover.structure.mutable.CNF

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

