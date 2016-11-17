package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause }
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Propagated(literal: Literal,
                      ancestors: Seq[SetClause],
                      reverseImpGraph: Map[Literal, Set[(SetClause, Seq[(Literal, Substitution)])]])
