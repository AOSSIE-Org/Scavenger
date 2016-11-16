package au.aossie.scavenger.proof
package cr

import language.implicitConversions

class CRProof(root: CRProofNode) extends Proof[CRProofNode](root)

object CRProof {
  implicit def apply(root: CRProofNode) = new CRProof(root)
}
