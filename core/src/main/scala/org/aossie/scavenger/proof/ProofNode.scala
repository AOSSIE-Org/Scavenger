package org.aossie.scavenger.proof

import org.aossie.scavenger.structure.immutable.Clause

import annotation.tailrec
import collection.mutable.{Stack, HashSet => MSet}

abstract class ProofNode[+J <: Clause, +P <: ProofNode[J,P]]
{
  def name = {val fullName = getClass.getName; fullName.substring(fullName.lastIndexOf('.' : Int) + 1)}
  private val self = asInstanceOf[P]
  def premises: Seq[P]
  def conclusion : J
  def parameters: Seq[Any] = Nil

  def existsAmongAncestors(predicate: (P) => Boolean): Boolean = {
    val visited = MSet(self)

    // As tail recursion is impossible, the fastest solution is to implement our own stack
    val todo = new Stack[P]()

    def pushPremises(node: P): Unit =
      for (prem <- node.premises if !visited(prem)) {
        visited += prem
        todo.push(prem)
      }
    pushPremises(self)

    @tailrec def visit(): Boolean =
      if (todo.isEmpty)
        false
      else {
        val node = todo.pop
        if (predicate(node))
          true
        else {
          pushPremises(node)
          visit()
        }
      }
    visit()
  }

}

//trait GenNullary[+J <: Sequent, +P <: ProofNode[J,P]] extends ProofNode[J,P] { def premises = Seq() }

trait GenUnary[+J <: Clause, +P <: ProofNode[J,P]] extends ProofNode[J,P] {
  def premise: P
  def premises = Seq(premise)
}

trait GenBinary[+J <: Clause, +P <: ProofNode[J,P]] extends ProofNode[J,P] {
  def leftPremise: P
  def rightPremise: P
  def premises = Seq(leftPremise, rightPremise)
}

