package org.aossie.scavenger.prover.heuristic

import org.aossie.scavenger.structure.immutable.Literal

import scala.collection.mutable

/**
  * Represents decision level.
  * Contains all information, needed to do a backtracking.
  *
  * @author Daniyar Itegulov
  */
class DecisionLevel(val literal: Literal,
                    val varAssessment: mutable.Set[Literal] = mutable.Set.empty)
