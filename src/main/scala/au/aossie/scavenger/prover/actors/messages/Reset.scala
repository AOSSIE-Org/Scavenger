package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.prover.Clause

/**
  * @author Daniyar Itegulov
  */
case class Reset(allClauses: Set[Clause])
