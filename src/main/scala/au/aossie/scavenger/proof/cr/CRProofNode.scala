package au.aossie.scavenger.proof
package cr

import collection.mutable.{HashMap => MMap, HashSet => MSet}
import au.aossie.scavenger.structure.immutable.SeqClause
import au.aossie.scavenger.expression.E

abstract class CRProofNode extends ProofNode[SeqClause, CRProofNode]

