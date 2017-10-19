package org.aossie.scavenger.prover.conflict

import org.aossie.scavenger.prover.heuristic.DecisionLevel
import org.aossie.scavenger.structure.immutable.Clause

/**
  * Very dumb algorithm, which just unions all literals on each decision level.
  *
  * @author Daniyar Itegulov
  */
object SimpleConflictAnalyser extends ConflictAnalyser{
  override def learnConflictClause(levels: Seq[DecisionLevel]): Clause =
    (Clause.empty /: levels.map(!_.literal)) (_ union _.toClause)
}

