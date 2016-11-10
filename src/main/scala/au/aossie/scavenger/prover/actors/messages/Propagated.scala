package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.structure.immutable.Literal
import au.aossie.scavenger.expression.substitution.immutable.Substitution

/**
  * @author Daniyar Itegulov
  */
case class Propagated(literal: Literal,
                      ancestors: Seq[Clause],
                      reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]])
