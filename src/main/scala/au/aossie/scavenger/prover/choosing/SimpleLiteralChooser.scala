package au.aossie.scavenger.prover.choosing

import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.structure.immutable.Literal
import au.aossie.scavenger.prover.structure.mutable.CNF

/**
  * Very dumb algorithm, which just pick some random undecided variable.
  *
  * @author Daniyar Itegulov
  */
object SimpleLiteralChooser extends LiteralChooser {
  override def chooseLiteral(cnf: CNF): Option[Literal] =
    cnf.variables.find(variable =>
      !cnf.assignment.contains(variable) && !cnf.assignment.contains(!variable)
    ).map(varToLit)
}
