package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause }
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Conflict(leftConflict: Literal,
                    rightConflict: Literal,
                    allClauses: Set[SetClause],
                    decisions: Seq[Literal],
                    reverseImpGraph: Map[Literal, Set[(SetClause, Seq[(Literal, Substitution)])]])
