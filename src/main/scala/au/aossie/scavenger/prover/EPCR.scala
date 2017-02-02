package au.aossie.scavenger.prover
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.structure.immutable.{CNF, Literal, SetClause => Clause}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * @author Daniyar Itegulov
  */
object EPCR extends Prover {

  val rnd = new Random(132374)

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    val propagatedLiterals = mutable.Set(cnf.clauses.filter(_.isUnit).map(_.literal): _*)
    val clauses = mutable.Set(cnf.clauses.filter(!_.isUnit): _*)
    val literals = mutable.Set(cnf.clauses.flatMap(_.literals): _*)
    val unifiableUnits = mutable.Map.empty[Literal, mutable.Set[Literal]]
    val reverseImplicationGraph = mutable.Map.empty[Clause, ArrayBuffer[CRProofNode]]
    val decisions = ArrayBuffer.empty[Literal]
    val conflictClauses = mutable.Set.empty[CRProofNode]
    val resolvedCache = mutable.Set.empty[(Clause, Int, Seq[Literal])]

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
      literals ++= newLiterals
      propagatedLiterals ++= newLiterals
      for (literal <- literals) {
        val set = unifiableUnits.getOrElseUpdate(literal, mutable.Set.empty)
        for (newLiteral <- newLiterals) if (newLiteral.negated != literal.negated) {
          unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
            case Some(_) => set += newLiteral
            case None    =>
          }
        }
      }
    }

    def resolve(clause: Clause, result: mutable.Set[Literal]): Unit = {
      val unifyCandidates = clause.literals.map(unifiableUnits(_).toSeq)
      for (conclusionId <- unifyCandidates.indices) {
        val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
        val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)
        for (unifier <- combinations(unifiers)) if (!resolvedCache.contains((clause, conclusionId, unifier))) {
          val task = (clause, conclusionId, unifier)
          val unifierUnits = unifier.map(_.unit)
          val literalUnits = literals.map(_.unit)
          resolvedCache += task
          if (unifyWithRename(unifierUnits, literalUnits).isDefined) {
            val clauseNode = reverseImplicationGraph(clause).head
            val unifierNodes = unifier.map(l => reverseImplicationGraph(l.toSetSequent).head)
            val unitPropagationNode = UnitPropagationResolution(unifierNodes, clauseNode, clause.literals(conclusionId), literals)
            val newLiteral = unitPropagationNode.conclusion.literal
              if (!result.contains(newLiteral) && !propagatedLiterals.contains(newLiteral)) {
                val buffer = reverseImplicationGraph.getOrElseUpdate(newLiteral, ArrayBuffer.empty)
                buffer += unitPropagationNode
                result += newLiteral
              }
          }
        }
      }
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      resolvedCache.clear()
      conflictClauses ++= newClauses
      unifiableUnits.clear()
      literals.clear()
      literals ++= cnf.clauses.flatMap(_.literals) ++ conflictClauses.map(_.conclusion).flatMap(_.literals)
      propagatedLiterals.clear()
      decisions.clear()
      reverseImplicationGraph.clear()
      cnf.clauses.foreach(clause =>
        reverseImplicationGraph.getOrElseUpdate(clause, ArrayBuffer.empty) += Axiom(clause))
      conflictClauses.foreach(node =>
        reverseImplicationGraph.getOrElseUpdate(node.conclusion, ArrayBuffer.empty) += node)
      propagatedLiterals ++= cnf.clauses.filter(_.isUnit).map(_.literal)
      propagatedLiterals ++= conflictClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)
      updateUnifiableUnits(propagatedLiterals.toSeq)
    }

    def removeDecisionLiteral(decisionLiteral: Literal): Unit = {
      decisions -= decisionLiteral
      def valid(node: CRProofNode): Boolean = {
        node match {
          case Decision(literal) if literal == decisionLiteral =>
            false
          case Decision(_) =>
            true
          case Axiom(_) =>
            true
          case ConflictDrivenClauseLearning(_) =>
            true
          case UnitPropagationResolution(left, right, _, _, _) =>
            left.forall(valid) && valid(right)
        }
      }
      for (literal <- propagatedLiterals) {
        reverseImplicationGraph(literal) = reverseImplicationGraph(literal).filter(valid)
      }
      val nonValidLiterals = reverseImplicationGraph.filter(_._2.isEmpty).keys.map(_.literal)
      propagatedLiterals --= nonValidLiterals
      unifiableUnits.values.foreach(_ --= nonValidLiterals)
    }

    updateUnifiableUnits(propagatedLiterals.toSeq)

    cnf.clauses.foreach(clause => reverseImplicationGraph(clause) = ArrayBuffer(Axiom(clause)))

    while (true) {
      val result = mutable.Set.empty[Literal]
      for (clause <- clauses) {
        resolve(clause, result)
      }
      for (conflictClause <- conflictClauses) if (!conflictClause.conclusion.isUnit) {
        resolve(conflictClause.conclusion, result)
      }

      println("Resolved:\n" + result.mkString("\n"))

      updateUnifiableUnits(result.toSeq)

      val CDCLClauses = mutable.Set.empty[CRProofNode]
      val allCDCLClauses = mutable.Set.empty[CRProofNode]
      propagatedLiterals.filter(unifiableUnits(_).nonEmpty).foreach { conflictLiteral =>
        for {
          otherLiteral <- unifiableUnits(conflictLiteral)
          conflictNode <- reverseImplicationGraph(conflictLiteral)
          otherNode    <- reverseImplicationGraph(otherLiteral)
          conflict = Conflict(conflictNode, otherNode)
        } {
          val cdclNode  = ConflictDrivenClauseLearning(conflict)
          val newClause = cdclNode.conclusion
          if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
          if (!cnf.clauses.contains(newClause) && !conflictClauses.exists(_.conclusion == newClause)) {
            CDCLClauses += cdclNode
          }
          allCDCLClauses += cdclNode
        }
      }

      if (CDCLClauses.nonEmpty) {
//        println("Resetting with:\n" + CDCLClauses.map(_.conclusion).mkString("\n"))
        reset(CDCLClauses.toSet)
      } else if (result.isEmpty) {
        val available = rnd.shuffle((literals -- propagatedLiterals -- propagatedLiterals.map(!_)).toSeq)
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          val decisionLiteral = available.head
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiteral(!decisionLiteral)
          }
          println("Decision: " + decisionLiteral)
          reverseImplicationGraph(decisionLiteral) = ArrayBuffer(Decision(decisionLiteral))
          updateUnifiableUnits(Seq(decisionLiteral))
        }
      } else if (allCDCLClauses.isEmpty && cnf.clauses.forall(clause => clause.literals.exists(propagatedLiterals.contains))) {
        val literals      = propagatedLiterals ++ decisions
        val trueLiterals  = literals.filterNot(_.negated).map(_.unit).toSet
        val falseLiterals = literals.filter(_.negated).map(_.unit).map(x => Neg(x)).toSet
        return Satisfiable(Some(new Assignment(trueLiterals ++ falseLiterals)))
      }
    }
    Error // this line is unreachable.
  }
  // scalastyle:on
}