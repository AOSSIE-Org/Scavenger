package au.aossie.scavenger.prover.conflict

import au.aossie.scavenger.structure.immutable.{ SetClause, SeqClause }
import au.aossie.scavenger.prover.util.DecisionLevel

/**
  * Represents general way to learn a conflict clause.
  *
  * @author Daniyar Itegulov
  */
trait ConflictAnalyser {
  /**
    * Predicts the best conflict clause.
    *
    * @param levels decision levels
    * @return learnt clause
    */
  def learnConflictClause(levels: Seq[DecisionLevel]): SeqClause
}

