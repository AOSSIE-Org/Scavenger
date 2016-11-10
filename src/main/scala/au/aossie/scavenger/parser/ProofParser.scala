package au.aossie.scavenger.parser

import au.aossie.scavenger.proof.{Proof,ProofNode}
import au.aossie.scavenger.judgment.Judgment

abstract class ProofParser[N <: ProofNode[Judgment,N]] {
  def read(filename: String) : Proof[N] 
}

