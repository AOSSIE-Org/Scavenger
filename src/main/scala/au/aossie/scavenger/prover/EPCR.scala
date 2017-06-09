package au.aossie.scavenger.prover
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.structure.immutable.{CNF, Literal, Clause}

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
    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(Axiom(Clause.empty))))
    }

    /**
      * Mutable set of proved literals initialized with the input CNF's unit clauses.
      */
    val provedLiterals: mutable.Set[Literal] = mutable.Set(cnf.clauses.filter(_.isUnit).map(_.literal): _*)

    /**
      * Non-unit clauses from the input CNF plus CDCL clauses.
      */
    var nonUnitClauses: Seq[Clause] = cnf.clauses.filter(!_.isUnit)

    /**
      * All literals used in `nonUnitClauses`.
      */
    var literals: Set[Literal] = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

    /**
      * Mutable map showing which value-literals can be unified with the key-literal.
      */
    val unifiableUnits: mutable.Map[Literal, mutable.Set[Literal]] = mutable.Map.empty

    /**
      * Mutable map showing all possible proofs for every proved clause.
      */
    val reverseImplicationGraph: mutable.Map[Clause, ArrayBuffer[CRProofNode]] = mutable.Map.empty


    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Set of clauses proved using CDCL rule.
      */
    val cdclClauses: mutable.Set[CRProofNode] = mutable.Set.empty

    /**
      * Stores combinations of (clause,
      */
    val resolvedCache = mutable.Set.empty[(Clause, Int, Seq[Literal])]

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
      literals ++= newLiterals
      provedLiterals ++= newLiterals
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
        val isUseful = (
          for (i <- unifyCandidates.indices) yield {
            i == conclusionId || unifyCandidates(i).nonEmpty
          }
          ).forall(identity)
        if (isUseful) {
          val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
          val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)
          for (unifier <- combinations(unifiers)) if (!resolvedCache.contains((clause, conclusionId, unifier))) {
            val task = (clause, conclusionId, unifier)
            val unifierUnits = unifier.map(_.unit)
            val literalUnits = literals.map(_.unit)
            resolvedCache += task
            if (unifyWithRename(unifierUnits, literalUnits).isDefined) {
              val clauseNode = reverseImplicationGraph(clause).head
              val unifierNodes = unifier.map(l => reverseImplicationGraph(l.toClause).head)
              val unitPropagationNode = UnitPropagationResolution(unifierNodes, clauseNode, clause.literals(conclusionId), literals)
              // TODO: Inside UnitPropagationResolution we redo the same unification that is done in "unifyWithRename". We could probably double the efficiency by avoid this duplicate computation somehow, but this would require a major refactor. It is better to leave it as it is now.
              val newLiteral = unitPropagationNode.conclusion.literal
              if (!result.contains(newLiteral) && !provedLiterals.contains(newLiteral)) {
                val buffer = reverseImplicationGraph.getOrElseUpdate(newLiteral, ArrayBuffer.empty)
                buffer += unitPropagationNode
                result += newLiteral
              }
            }
          }
        }
      }
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      resolvedCache.clear()
      cdclClauses ++= newClauses
      nonUnitClauses = nonUnitClauses ++ newClauses.map(_.conclusion).filter(!_.isUnit)
      unifiableUnits.clear()
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      decisions.clear()
      reverseImplicationGraph.clear()
      cnf.clauses.foreach(clause =>
        reverseImplicationGraph.getOrElseUpdate(clause, ArrayBuffer.empty) += Axiom(clause))
      cdclClauses.foreach(node =>
        reverseImplicationGraph.getOrElseUpdate(node.conclusion, ArrayBuffer.empty) += node)
      provedLiterals.clear()
      provedLiterals ++= cnf.clauses.filter(_.isUnit).map(_.literal)
      provedLiterals ++= cdclClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)
      updateUnifiableUnits(provedLiterals.toSeq)
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
      for (literal <- provedLiterals) {
        reverseImplicationGraph(literal) = reverseImplicationGraph(literal).filter(valid)
      }
      val nonValidLiterals = reverseImplicationGraph.filter(_._2.isEmpty).keys.map(_.literal)
      provedLiterals --= nonValidLiterals
      unifiableUnits.values.foreach(_ --= nonValidLiterals)
    }

    def removeConflictDecisions(node: CRProofNode): Unit = {
      node match {
        case Decision(literal) =>
          removeDecisionLiteral(literal)
        case conflict @ Conflict(left, right) =>
          removeConflictDecisions(left)
          removeConflictDecisions(right)
        case UnitPropagationResolution(left, _, _, _, _) =>
          left.foreach(removeConflictDecisions)
        case ConflictDrivenClauseLearning(conflict) =>
          removeConflictDecisions(conflict)
        case _ =>
      }
    }

    def addCDCLClauses(newClauses: Set[CRProofNode]): Unit = {
      resolvedCache.clear()
      cdclClauses ++= newClauses
      nonUnitClauses = nonUnitClauses ++ newClauses.map(_.conclusion).filter(!_.isUnit)
      unifiableUnits.clear()
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      newClauses.foreach(node =>
        reverseImplicationGraph.getOrElseUpdate(node.conclusion, ArrayBuffer.empty) += node)
      updateUnifiableUnits(provedLiterals.toSeq)
    }

    updateUnifiableUnits(provedLiterals.toSeq)

    cnf.clauses.foreach(clause => reverseImplicationGraph(clause) = ArrayBuffer(Axiom(clause)))

    while (true) {
      val result = mutable.Set.empty[Literal]
      for (clause <- nonUnitClauses) if (!clause.literals.exists(provedLiterals.contains)) {
        resolve(clause, result)
      }

      updateUnifiableUnits(result.toSeq)

      val CDCLClauses = mutable.Set.empty[CRProofNode]
      provedLiterals.filter(unifiableUnits(_).nonEmpty).foreach { conflictLiteral =>
        for {
          otherLiteral <- unifiableUnits(conflictLiteral)
          conflictNode <- reverseImplicationGraph(conflictLiteral)
          otherNode    <- reverseImplicationGraph(otherLiteral)
          conflict = Conflict(conflictNode, otherNode)
        } {
          val cdclNode  = ConflictDrivenClauseLearning(conflict)
          val newClause = cdclNode.conclusion
          if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
          CDCLClauses += cdclNode
        }
      }

      if (CDCLClauses.nonEmpty) {
//        reset(CDCLClauses.toSet)
        addCDCLClauses(CDCLClauses.toSet)
        CDCLClauses.foreach(removeConflictDecisions)
      } else if (result.isEmpty) {
        val available = rnd.shuffle((literals -- provedLiterals -- provedLiterals.map(!_)).toSeq)
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          val decisionLiteral = available.head
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiteral(!decisionLiteral)
          }
          reverseImplicationGraph(decisionLiteral) = ArrayBuffer(Decision(decisionLiteral))
          updateUnifiableUnits(Seq(decisionLiteral))
        }
      } else if (cnf.clauses.forall(clause => clause.literals.exists(provedLiterals.contains))) {
        val literals      = provedLiterals ++ decisions
        val trueLiterals  = literals.filterNot(_.negated).map(_.unit).toSet
        val falseLiterals = literals.filter(_.negated).map(_.unit).map(x => Neg(x)).toSet
        return Satisfiable(Some(new Assignment(trueLiterals ++ falseLiterals)))
      }
    }
    Error // this line is unreachable.
  }
  // scalastyle:on
}
