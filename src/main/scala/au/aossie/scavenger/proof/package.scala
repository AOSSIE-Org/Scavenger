package au.aossie.scavenger

import au.aossie.scavenger.structure.AbstractClause
import au.aossie.scavenger.util.math.max
import scala.collection.mutable.{HashMap => MMap,HashSet => MSet}
import au.aossie.scavenger.proof.cr.CRProofNode

package object proof {
  def measure[N <: ProofNode[AbstractClause,N]](p: Proof[N]) = {
    var length = 0
    var coreSize = 0
    val childrenVisited = MMap[N,Int]()
    val height =
      p foldDown { (n,heights:Seq[Int]) => 
        var step = 1
        n.premises.foreach(pr => {
          val chV = childrenVisited.getOrElse(pr, 0) + 1
          childrenVisited.update(pr, chV)
          if (chV == p.childrenOf(pr).size) {
            step = step - 1
          }
        })
        length += 1

        if (n.premises.length == 0) coreSize += 1
        max(heights, (x:Int)=>x, default = 0) + 1
      } 
    Map("length" -> length, "coreSize" -> coreSize, "height" -> height)
    //, "transLength" -> transLength) for Congruence algorithm
  }
}
