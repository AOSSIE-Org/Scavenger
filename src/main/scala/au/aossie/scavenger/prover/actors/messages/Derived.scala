package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause }
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr

/**
  * @author Daniyar Itegulov
  */
case class Derived(clause: SetClause,
                   reverseImpGraph: Map[Literal, Set[(SetClause, Seq[(Literal, Substitution)])]],
                   conflict: cr.Conflict)
