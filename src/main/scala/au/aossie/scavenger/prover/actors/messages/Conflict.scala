package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{Literal,SeqClause}
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Conflict(leftConflict: Literal,
                    rightConflict: Literal,
                    allClauses: Set[SeqClause],
                    decisions: Seq[Literal],
                    reverseImpGraph: Map[Literal, Set[(SeqClause, Seq[(Literal, Substitution)])]])
