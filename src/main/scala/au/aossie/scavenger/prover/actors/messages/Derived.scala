package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ Literal, SeqClause }
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr

/**
  * @author Daniyar Itegulov
  */
case class Derived(clause: SeqClause,
                   reverseImpGraph: Map[Literal, Set[(SeqClause, Seq[(Literal, Substitution)])]],
                   conflict: cr.Conflict)
