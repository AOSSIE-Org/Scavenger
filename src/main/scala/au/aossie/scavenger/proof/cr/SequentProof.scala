package au.aossie.scavenger.proof
package cr

import collection.mutable.{HashMap => MMap, HashSet => MSet}
import au.aossie.scavenger.structure.immutable.{Clause => Sequent}
import au.aossie.scavenger.expression.E

abstract class SequentProofNode
extends ProofNode[Sequent, SequentProofNode] {
  //def auxFormulasMap: Map[SequentProofNode, Sequent]
  //def mainFormulas : Sequent
  //def conclusionContext : Sequent
  // The lazy modifier for "conclusion" is very important,
  // because "conclusion" calls methods that will only be overriden by subtraits and subclasses.
  //override lazy val conclusion: Sequent = mainFormulas union conclusionContext
  //override def toString():String = {
  //  this.conclusion.toString
  //}
}

//trait Binary extends SequentProofNode with GenBinary[Sequent,SequentProofNode] {  
//  def leftAuxFormulas: Sequent
//  def rightAuxFormulas: Sequent
//  def auxFormulasMap = Map(leftPremise -> leftAuxFormulas, rightPremise -> rightAuxFormulas)
//}

