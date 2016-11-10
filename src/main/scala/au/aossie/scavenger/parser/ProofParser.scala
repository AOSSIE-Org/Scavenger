package au.aossie.scavenger.parser

import au.aossie.scavenger.proof.{Proof,ProofNode}
import au.aossie.scavenger.structure.Sequent

abstract class ProofParser[N <: ProofNode[Sequent,N]] {
  def read(filename: String) : Proof[N] 
}

