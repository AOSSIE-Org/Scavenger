package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{Literal,Clause}
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr

/**
  * @author Daniyar Itegulov
  */
case class Derived(clause: Clause,
                   reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]],
                   conflict: cr.Conflict)
