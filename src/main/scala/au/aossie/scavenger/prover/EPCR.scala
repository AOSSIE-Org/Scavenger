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
    // TODO: not only in nonUnitClauses but in all proved clauses
    var literals: Set[Literal] = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

    /**
      * Mutable map showing which value-literals can be unified with the key-literal.
      */
    // TODO: is it necessarily to use Ids and Buff?
    val unifiableUnitsIds: mutable.Map[Literal, Int] = mutable.Map.empty
    val unifiableUnitsBuff: ArrayBuffer[mutable.Set[Literal]] = mutable.ArrayBuffer.empty

    /**
      * Mutable map showing all possible proofs for every proved clause.
      */
    // TODO: is it necessarily so many structures for one nature?
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

    val MAX_UNIFIERS_COUNT = 20

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {

      literals ++= newLiterals

      for (literal <- literals) {
        val indexByLiteral = unifiableUnitsIds.getOrElseUpdate(literal, unifiableUnitsBuff.size)
        if (indexByLiteral == unifiableUnitsBuff.size) {
          unifiableUnitsBuff += mutable.Set.empty
        }
        val set = unifiableUnitsBuff(indexByLiteral)
        // TODO: analyze randomness here
        for (newLiteral <- rnd.shuffle(newLiterals)) {
          if ((set.size < MAX_UNIFIERS_COUNT) && (newLiteral.negated != literal.negated)) {
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


          /**
            * Recursively generates UnitPropagationResolution checking unification on each step.
            *
            * @param unifier chosen literals
            * @param subs substitutions for literals to ensure that variables in different literals are different
            * @param literalsWithSubst literals with unique variables
            * @param globalSubst same global MGU for all literals
            * @param cur number of chosen literals
            */
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
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

      decisions.clear()

      reverseImplication.clear()
      bufferNodes.clear()
      cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))
      cdclClauses.foreach(node => addNode(node.conclusion, node))

      provedLiterals.clear()
      provedLiterals ++= cnf.clauses.filter(_.isUnit).map(_.literal)
      provedLiterals ++= cdclClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)

      unifiableUnitsIds.clear()
      unifiableUnitsBuff.clear()
      updateUnifiableUnits(provedLiterals.toSeq)
    }

    def removeDecisionLiterals(decisionLiterals: mutable.HashSet[Literal]): Unit = {
      decisions --= decisionLiterals

      def valid(node: CRProofNode): Boolean = {
        node match {
          case Decision(literal)  =>
            decisions.contains(literal)
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

    def getAllConflictDecisions(node: CRProofNode, acc: mutable.Set[Literal]): Unit = {
      node match {
        case Decision(literal) =>
//          removeDecisionLiteral(literal)
          acc += literal
        case conflict @ Conflict(left, right) =>
          getAllConflictDecisions(left, acc)
          getAllConflictDecisions(right, acc)
        case UnitPropagationResolution(left, right, _, _, _) =>
          left.foreach(getAllConflictDecisions(_, acc))
          getAllConflictDecisions(right, acc)
        case ConflictDrivenClauseLearning(conflict) =>
          getAllConflictDecisions(conflict, acc)
        case Axiom(_) =>
      }
    }

    def addCDCLClauses(newClauses: Set[CRProofNode]): Unit = {
      cdclClauses ++= newClauses
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

      newClauses.foreach(node =>
        addNode(node.conclusion, node))

      val newProvedLiterals = newClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)
      provedLiterals ++= newProvedLiterals
      updateUnifiableUnits(newProvedLiterals.toSeq)
    }

    // already proved literals
    updateUnifiableUnits(provedLiterals.toSeq)

    cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))

    while (true) {
      val result = mutable.Set.empty[Literal]
      for (clause <- nonUnitClauses)
        if (!clause.literals.exists(provedLiterals.contains)) { // if clause has not already proven
          resolve(clause, result)
        }

      provedLiterals ++= result
      updateUnifiableUnits(result.toSeq)

      // find clauses of kind `A & !B` where there is some unification for {A = B}
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
//        reset(CDCLClauses.toSet)
        val conflictLiterals: mutable.HashSet[Literal] = mutable.HashSet.empty
        CDCLClauses.foreach(getAllConflictDecisions(_, conflictLiterals))
        removeDecisionLiterals(conflictLiterals)
        addCDCLClauses(CDCLClauses.toSet)
      } else if (result.isEmpty) {
        val available = rnd.shuffle((literals -- provedLiterals -- provedLiterals.map(!_)).toSeq)
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          val decisionLiteral = available.head
          println(decisionLiteral)
          provedLiterals += decisionLiteral
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiterals(mutable.HashSet(!decisionLiteral))
          }
          addNode(decisionLiteral, Decision(decisionLiteral))
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
