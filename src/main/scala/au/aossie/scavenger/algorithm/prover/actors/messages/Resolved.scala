package au.aossie.scavenger.algorithm.prover.actors.messages

import au.aossie.scavenger.algorithm.prover._
import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Resolved(reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]])
