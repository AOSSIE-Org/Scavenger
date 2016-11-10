package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.Clause

/**
  * @author Daniyar Itegulov
  */
case class Reset(allClauses: Set[Clause])
