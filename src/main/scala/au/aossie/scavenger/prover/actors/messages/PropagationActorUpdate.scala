package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{Literal,SeqClause}

/**
  * @author Daniyar Itegulov
  */
case class PropagationActorUpdate(newClauses: Set[SeqClause], newUnifiableUnits: Map[Literal, Set[Literal]])
