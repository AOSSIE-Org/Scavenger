package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.{ SeqClause, SetClause }

/**
  * @author Daniyar Itegulov
  */
case class Reset(allClauses: Set[SetClause])
