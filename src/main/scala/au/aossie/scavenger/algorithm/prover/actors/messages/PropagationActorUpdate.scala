package au.aossie.scavenger.algorithm.prover.actors.messages

import au.aossie.scavenger.algorithm.prover.Clause
import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal

/**
  * @author Daniyar Itegulov
  */
case class PropagationActorUpdate(newClauses: Set[Clause], newUnifiableUnits: Map[Literal, Set[Literal]])
