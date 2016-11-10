package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{Literal,Clause}

/**
  * @author Daniyar Itegulov
  */
case class PropagationActorUpdate(newClauses: Set[Clause], newUnifiableUnits: Map[Literal, Set[Literal]])
