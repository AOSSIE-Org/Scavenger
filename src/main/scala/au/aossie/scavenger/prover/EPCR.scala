package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.{E, Var}
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import au.aossie.scavenger.unification.{ MartelliMontanari => unify }


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
    val unifiableUnitsIds: mutable.Map[Literal, Int] = mutable.Map.empty

    val unifiableUnitsBuff: ArrayBuffer[mutable.Set[Literal]] = mutable.ArrayBuffer.empty

    /**
      * Mutable map showing all possible proofs for every proved clause.
      */
    val reverseImplication: mutable.Map[Clause, Int] = mutable.Map.empty

    val bufferNodes: ArrayBuffer[ArrayBuffer[CRProofNode]] = mutable.ArrayBuffer.empty
    val nodeById: ArrayBuffer[Clause] = mutable.ArrayBuffer.empty

    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Set of clauses proved using CDCL rule.
      */
    val cdclClauses: mutable.Set[CRProofNode] = mutable.Set.empty

    val MAX_UNIFIERS_CNT = 20

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
      literals ++= newLiterals
      provedLiterals ++= newLiterals

      for (literal <- literals) {
        val id = unifiableUnitsIds.getOrElseUpdate(literal, unifiableUnitsBuff.size)
        if (id == unifiableUnitsBuff.size) {
          unifiableUnitsBuff += mutable.Set.empty
        }
        val set = unifiableUnitsBuff(id)
        for (newLiteral <- rnd.shuffle(newLiterals)) {
          if ((set.size < MAX_UNIFIERS_CNT) && (newLiteral.negated != literal.negated)) {
            unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
              case Some(_) =>
                set += newLiteral
              case None =>
            }
          }
        }
      }
    }

    def addNode(cl: Clause, node: CRProofNode): Unit = {
      val reverseId = reverseImplication.getOrElseUpdate(cl, bufferNodes.size)
      if (reverseId == bufferNodes.size) {
        bufferNodes += ArrayBuffer.empty
        nodeById += cl
      }
      bufferNodes(reverseId) += node
    }

    def resolve(clause: Clause, result: mutable.Set[Literal]): Unit = {
      val unifyCandidates = clause.literals.map(id => unifiableUnitsBuff(unifiableUnitsIds(id)).toSeq)
      for (conclusionId <- unifyCandidates.indices) {
        val isUseful = (
          for (i <- unifyCandidates.indices) yield {
            i == conclusionId || unifyCandidates(i).nonEmpty
          }
          ).forall(identity)

        if (isUseful) {
          val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
          val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)

          def go(unifier: Seq[Literal],
                 unifierWithSub: Seq[E],
                 subs: Seq[Substitution],
                 literalsWithSubst: Seq[Literal],
                 globalSubst: Substitution,
                 usedVars: Set[Var],
                 cur: Int): Unit = {

            if (cur == unifiers.size) {
              val clauseNode = bufferNodes(reverseImplication(clause)).head
              val unifierNodes = unifier.map(l => bufferNodes(reverseImplication(l.toClause)).head)
              val unitPropagationNode =
                UnitPropagationResolution(
                  unifierNodes,
                  clauseNode,
                  clause.literals(conclusionId),
                  literals,
                  subs,
                  globalSubst
                )
              val newLiteral = unitPropagationNode.conclusion.literal
              if (!result.contains(newLiteral) && !provedLiterals.contains(newLiteral)) {
                addNode(newLiteral, unitPropagationNode)
                result += newLiteral
              }
              return
            }

            for (curUni <- unifiers(cur)) {
              val substitution = renameVars(globalSubst(curUni.unit), usedVars)
              val newSubs = subs :+ globalSubst(substitution)
              val leftWithSubst = substitution(globalSubst(curUni.unit))
              val newUnifierWithSubst = unifierWithSub :+ leftWithSubst

              val unificationSubst = unify(leftWithSubst, literalsWithSubst(cur).unit)
              if (unificationSubst.isDefined) {
                go(unifier :+ curUni,
                  newUnifierWithSubst.map(unificationSubst.get(_)),
                  newSubs.map(_(unificationSubst.get)),
                  literalsWithSubst.map(unificationSubst.get(_)),
                  globalSubst(unificationSubst.get),
                  usedVars ++ leftWithSubst.variables,
                  cur + 1
                )
              }
            }
          }

          go(Seq.empty,
            Seq.empty,
            Seq.empty,
            literals,
            Substitution.empty,
            literals.map(_.unit.variables.toSet).reduce { _ union _ },
            0)
        }
      }
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      cdclClauses ++= newClauses
      nonUnitClauses = nonUnitClauses ++ newClauses.map(_.conclusion).filter(!_.isUnit)
      unifiableUnitsIds.clear()
      unifiableUnitsBuff.clear()
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      decisions.clear()
      reverseImplication.clear()
      bufferNodes.clear()
      cnf.clauses.foreach(clause =>
        addNode(clause, Axiom(clause)))
//        reverseImplicationGraph.getOrElseUpdate(clause, mutable.Set.empty) += Axiom(clause))
      cdclClauses.foreach(node =>
        addNode(node.conclusion, node))
//        reverseImplicationGraph.getOrElseUpdate(node.conclusion, mutable.Set.empty) += node)
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
        val reverseId = reverseImplication(literal)
        bufferNodes(reverseId) = bufferNodes(reverseId).filter(valid)
      }
      val nonValidLiterals = bufferNodes.zipWithIndex.filter(_._1.isEmpty).map(_._2).map(nodeById(_).literal)
      provedLiterals --= nonValidLiterals
      literals --= nonValidLiterals
      unifiableUnitsBuff.foreach(_ --= nonValidLiterals)
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
        case ConflictDrivenClauseLearning(_) =>
        case Axiom(_) =>
      }
    }

    def addCDCLClauses(newClauses: Set[CRProofNode]): Unit = {
      cdclClauses ++= newClauses
      nonUnitClauses = nonUnitClauses ++ newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      newClauses.foreach(node =>
        addNode(node.conclusion, node))
//        reverseImplicationGraph.getOrElseUpdate(node.conclusion, mutable.Set.empty) += node)
      unifiableUnitsBuff.clear()
      unifiableUnitsIds.clear()
      updateUnifiableUnits(provedLiterals.toSeq)
    }

    updateUnifiableUnits(provedLiterals.toSeq)

    cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))

    while (true) {
      val result = mutable.Set.empty[Literal]
      for (clause <- nonUnitClauses) if (!clause.literals.exists(provedLiterals.contains)) {
        resolve(clause, result)
      }

      val resultSeq = result.toSeq
      updateUnifiableUnits(resultSeq)

      val CDCLClauses = mutable.Set.empty[CRProofNode]
      provedLiterals.filter(l => unifiableUnitsBuff(unifiableUnitsIds(l)).nonEmpty).foreach { conflictLiteral =>
        for {
          otherLiteral <- unifiableUnitsBuff(unifiableUnitsIds(conflictLiteral))
          conflictNode <- bufferNodes(reverseImplication(conflictLiteral))
          otherNode    <- bufferNodes(reverseImplication(otherLiteral))
          conflict = Conflict(conflictNode, otherNode)
        } {
          val cdclNode  = ConflictDrivenClauseLearning(conflict)
          val newClause = cdclNode.conclusion
          if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
          CDCLClauses += cdclNode
        }
      }

      if (CDCLClauses.nonEmpty) {
        reset(CDCLClauses.toSet)
//        addCDCLClauses(CDCLClauses.toSet)
//        CDCLClauses.foreach(removeConflictDecisions)
      } else if (result.isEmpty) {
        val available = rnd.shuffle((literals -- provedLiterals -- provedLiterals.map(!_)).toSeq)
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          for (avId <- 0 until 1.min(available.size)) {
            val decisionLiteral = available(avId)
            decisions += decisionLiteral
            if (decisions.contains(!decisionLiteral)) {
              removeDecisionLiteral(!decisionLiteral)
            }
            addNode(decisionLiteral, Decision(decisionLiteral))
            updateUnifiableUnits(Seq(decisionLiteral))
          }
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
