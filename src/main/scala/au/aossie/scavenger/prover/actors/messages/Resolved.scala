package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{Literal,SeqClause}
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Resolved(reverseImpGraph: Map[Literal, Set[(SeqClause, Seq[(Literal, Substitution)])]])
