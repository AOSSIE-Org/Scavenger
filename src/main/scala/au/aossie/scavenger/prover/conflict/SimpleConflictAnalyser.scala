package au.aossie.scavenger.prover.conflict

import au.aossie.scavenger.prover.util.DecisionLevel
import au.aossie.scavenger.structure.immutable.{ SetClause, SeqClause }

/**
  * Very dumb algorithm, which just unions all literals on each decision level.
  *
  * @author Daniyar Itegulov
  */
object SimpleConflictAnalyser extends ConflictAnalyser{
  override def learnConflictClause(levels: Seq[DecisionLevel]): SeqClause =
    (SeqClause.empty /: levels.map(!_.literal)) (_ union _.toClause)
}

