package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover.Clause
import au.aossie.scavenger.prover.structure.immutable.Literal

/**
  * @author Daniyar Itegulov
  */
case class PropagationActorUpdate(newClauses: Set[Clause], newUnifiableUnits: Map[Literal, Set[Literal]])
