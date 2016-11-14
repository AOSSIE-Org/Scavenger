package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.SeqClause

/**
  * @author Daniyar Itegulov
  */
case class Reset(allClauses: Set[SeqClause])
