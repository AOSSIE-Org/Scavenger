package org.aossie.scavenger.prover.conflict

import org.aossie.scavenger.structure.immutable.Clause
import org.aossie.scavenger.prover.heuristic.DecisionLevel

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

