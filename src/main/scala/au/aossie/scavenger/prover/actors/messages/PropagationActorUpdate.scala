package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ Literal, SetClause }

/**
  * @author Daniyar Itegulov
  */
case class PropagationActorUpdate(newClauses: Set[SetClause], newUnifiableUnits: Map[Literal, Set[Literal]])
