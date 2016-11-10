package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover.Clause
import au.aossie.scavenger.prover.structure.immutable.Literal
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.sequent.conflictresolution

/**
  * @author Daniyar Itegulov
  */
case class Derived(clause: Clause,
                   reverseImpGraph: Map[Literal, Set[(Clause, Seq[(Literal, Substitution)])]],
                   conflict: conflictresolution.Conflict)
