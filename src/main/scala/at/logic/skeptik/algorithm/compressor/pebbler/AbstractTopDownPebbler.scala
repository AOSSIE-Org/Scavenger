package at.logic.skeptik.algorithm.compressor.pebbler

import at.logic.skeptik.proof.Proof
import at.logic.skeptik.proof.sequent.{SequentProofNode => N}
import scala.collection.mutable.{HashMap => MMap}
import scala.collection.mutable.{HashSet => MSet}
import scala.collection.mutable.ArrayBuffer

/**
 * Top down pebblers traverse the proof from leaves to the root and pebble nodes
 * using a specific heuristic in form of a node ordering.
 * 
 * By pebbling nodes, other nodes are made available for pebbling 
 * while initially only nodes without premises are available.
 * This is done until no more nodes are available for pebbling, i.e. the root node is pebbled.
 * Top-down pebbling directly corresponds to playing the Black Pebbling Game
 * */

abstract class AbstractTopDownPebbler extends AbstractPebbler  {
  
  def findProof(proof: Proof[N], nodeInfos: MMap[N,NodeInfo], reverseNode: Option[N]): Proof[N] = {
    //Pebbled nodes go into this Seq
    val permutation = new ArrayBuffer[N]()
    
    val canBePebbled:MSet[N] = MSet[N]()
    proof.filter(a => a.premises.isEmpty).foreach(canBePebbled.add(_))
    
//    proof.nodes.foreach(n => {
//      if (n.premises.isEmpty) canBePebbled += n
//    })
    
    while(!canBePebbled.isEmpty) {
      //Choose the next node as the maximum w.r.t. the used heuristic of pebbleable nodes
      val next = canBePebbled.max(usedOrder(proof,nodeInfos))
      
      permutation += next
//      print(nodeInfos(next).index + ", ")
      canBePebbled -= next
      
      //Update the relevant nodeInfo objects
      next.premises.foreach(pr => {
        if (nodeInfos.isDefinedAt(pr)) {
          val cNP = nodeInfos(pr).childrenNotPebbled - 1
          //This premise can be unpebbled.
          if (cNP == 1) {
//            nodeInfos -= pr
          }
          else {
            nodeInfos(pr) = nodeInfos(pr).changeChildrenNotPebbled(cNP)
          }
        }
      })
      //calculate an upper bound for the number of pebbles used for this node as 
      //the sum of the upper bounds of all premises plus 1
      val uses = (next.premises foldLeft 1) ((A,B) => 
        A + nodeInfos.getOrElse(B, EmptyNI).usesPebbles)
      nodeInfos(next) = nodeInfos(next).changeUsesPebbles(uses)
      
      //visit all children of the current node and decrement their waitsForPremises number by 1
      //if this number was 1 before for a children c, then c can be made available for pebbling
      proof.childrenOf(next).foreach(c => {
        val wF = nodeInfos(c).waitsForPremises
        nodeInfos(c) = nodeInfos(c).changeWaitsForPremises(wF - 1)
        if (wF == 1) {
          canBePebbled += c
        }
      })
      next.premises.foreach(pr => {
        proof.childrenOf(pr).foreach(c => nodeInfos(c) = nodeInfos(c).changeBlocked(true))
      })
      nodeInfos(next) = nodeInfos(next).changeWasPebbled(permutation.size)
    }
//    println()
    new Proof(proof.root, permutation.reverse.toIndexedSeq)
  }
}
