package org.aossie.scavenger.prover.choosing

import org.aossie.scavenger.structure.immutable._
import org.aossie.scavenger.structure.mutable.CNF

/**
  * Very dumb algorithm, which just pick some random undecided variable.
  *
  * @author Daniyar Itegulov
  */
object SimpleLiteralChooser extends LiteralChooser {
  override def chooseLiteral(cnf: CNF): Option[Literal] =
    cnf.variables.find(variable =>
      //TODO: I think there is a bug here, because unification is not being taken into account.
      // Suppose the assignment contains "not p(c)" but does not contain "p(X)" or "not p(X)".
      // Then this function could return "p(X)". But "p(X)" is inconsistent with "not p(c)".
      !cnf.assignment.contains(variable) && !cnf.assignment.contains(!variable)
    ).map(varToLit)
}

