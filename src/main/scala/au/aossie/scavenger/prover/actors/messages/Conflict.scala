package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.structure.immutable.Literal
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Conflict(leftConflict: Literal,
                    rightConflict: Literal,
                    allClauses: Set[Clause],
                    decisions: Seq[Literal],
                    reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]])
