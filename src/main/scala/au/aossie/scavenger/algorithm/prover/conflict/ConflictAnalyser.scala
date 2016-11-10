package au.aossie.scavenger.algorithm.prover.conflict

import au.aossie.scavenger.algorithm.prover.Clause
import au.aossie.scavenger.algorithm.prover.util.DecisionLevel

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
  def learnConflictClause(levels: Seq[DecisionLevel]): Clause
}
