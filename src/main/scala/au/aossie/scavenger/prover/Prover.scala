package au.aossie.scavenger.prover

import au.aossie.scavenger.proof.ProofNode
import au.aossie.scavenger.judgment.Judgment

abstract class Prover[J <: Judgment, P <: ProofNode[J,P]] {
  def calculus: Calculus[J,P]
  
  def prove(goal: J): Option[P]
}