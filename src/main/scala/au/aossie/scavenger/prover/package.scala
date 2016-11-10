package au.aossie.scavenger

import au.aossie.scavenger.proof.ProofNode
import au.aossie.scavenger.judgment.Judgment

package object prover {
  type Calculus[J <: Judgment, P <: ProofNode[J,P]] = Seq[InferenceRule[J, P]]
}