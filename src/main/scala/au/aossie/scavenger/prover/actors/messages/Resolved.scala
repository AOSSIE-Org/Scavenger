package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover._
import au.aossie.scavenger.structure.immutable.{Literal,Clause}
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Resolved(reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]])
