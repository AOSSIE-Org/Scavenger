package au.aossie.scavenger.algorithm.prover.actors.messages

import au.aossie.scavenger.algorithm.prover.Clause

/**
  * @author Daniyar Itegulov
  */
case class Reset(allClauses: Set[Clause])
