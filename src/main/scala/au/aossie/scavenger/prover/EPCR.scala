package au.aossie.scavenger.prover

import au.aossie.scavenger.expression.{E, Var}
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import au.aossie.scavenger.unification.{MartelliMontanari => unify}

import scala.util.control.Breaks


/**
  * @author Daniyar Itegulov
  */
object EPCR extends Prover {

  val rnd = new Random(107)
  val MAX_UNIFIERS_COUNT: Int = 50
  val MAX_CNT_WITHOUT_DECISIONS: Int = 10

  val BUMP: Double = 5.0
  val DECAY_FACTOR: Double = 0.9

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(Axiom(Clause.empty))))
    }

    /**
      * Mutable set of proved literals initialized with the input CNF's unit clauses.
      */
    val provedLiterals: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Non-unit clauses from the input CNF plus CDCL clauses.
      */
    var nonUnitClauses: Seq[Clause] = cnf.clauses.filter(!_.isUnit)

    /**
      * All literals used in `nonUnitClauses`.
      */
    // TODO: not only in nonUnitClauses but in all proved clauses
    var literals: mutable.Set[Literal] = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

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

    val activity: mutable.Map[Literal, Double] = mutable.HashMap.empty

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {

      literals.foreach(l => activity.update(l, activity.getOrElseUpdate(l, 0) * DECAY_FACTOR))
      newLiterals.foreach(l => activity.update(l, activity.getOrElseUpdate(l, 0) + BUMP))
      literals ++= newLiterals

      val newLiteralsShuffle = rnd.shuffle(newLiterals)

//      println(s"literals(${literals.size}), newLiterals(${newLiteralsShuffle.size})")

      for (literal <- literals) {
        val indexByLiteral = unifiableUnitsIds.getOrElseUpdate(literal, unifiableUnitsBuff.size)
        if (indexByLiteral == unifiableUnitsBuff.size) {
          unifiableUnitsBuff += mutable.Set.empty
        }
        val set = unifiableUnitsBuff(indexByLiteral)
        var cnt = 0
        // TODO: analyze randomness here
        Breaks.breakable {
          for (newLiteral <- newLiteralsShuffle) {
            if (newLiteral.negated != literal.negated) {
              unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
                case Some(_) =>
                  set += newLiteral
                  cnt += 1
                  if (cnt == MAX_UNIFIERS_COUNT) {
                    Breaks.break()
                  }
//                  if (set.size > MAX_UNIFIERS_COUNT) {
//                    set -= set.minBy(lit => activity(lit))
//                  }
                case None =>
              }
            }
          }
        }
      }
    }

    def addProvedLiterals(newProvedLiterals: Seq[Literal]) = {
      provedLiterals ++= newProvedLiterals
      updateUnifiableUnits(newProvedLiterals)
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

        if (unifyCandidates.indices.forall(i => i == conclusionId || unifyCandidates(i).nonEmpty)) {
          val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
          val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)


          /**
            * Recursively generates UnitPropagationResolution checking unification on each step.
            *
            * @param unifier           chosen literals
            * @param subs              substitutions for literals to ensure that variables in different literals are different
            * @param literalsWithSubst literals with unique variables
            * @param globalSubst       same global MGU for all literals
            * @param cur               number of chosen literals
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
              //              println(unifier.map(l => bufferNodes(reverseImplication(l.toClause))))
              val unifierNodes = unifier.map(l => bufferNodes(reverseImplication(l.toClause)).head)
              val curSubst = renameVars(clause.literals(conclusionId).unit, usedVars)
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
              val substitution = renameVars(curUni.unit, usedVars)
              val newSubs = subs :+ substitution(globalSubst)
              val leftWithSubst = globalSubst(substitution(curUni.unit))
              val newUnifierWithSubst = unifierWithSub :+ leftWithSubst

              val unificationSubst = unify(leftWithSubst, literalsWithSubst(cur).unit)
              if (unificationSubst.isDefined) {
                go(unifier :+ curUni,
                  newUnifierWithSubst.map(unificationSubst.get(_)),
                  newSubs.map(_ (unificationSubst.get)),
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
            literals.map(_.unit.variables.toSet).reduce {
              _ union _
            },
            0)
        }
      }
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      println("RESET")
      cdclClauses ++= newClauses
      nonUnitClauses = nonUnitClauses ++ newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses
        .flatMap(_.literals)(collection.breakOut)
      decisions.clear()

      reverseImplication.clear()
      bufferNodes.clear()

      cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))
      cdclClauses.foreach(node => addNode(node.conclusion, node))

      unifiableUnitsIds.clear()
      unifiableUnitsBuff.clear()

      provedLiterals.clear()
      addProvedLiterals(cnf.clauses.filter(_.isUnit).map(_.literal))
      addProvedLiterals(cdclClauses.toSeq.map(_.conclusion).filter(_.isUnit).map(_.literal))
      println(s"provedLiterals.size = ${provedLiterals.size}")
    }

    val mem: mutable.HashMap[CRProofNode, Boolean] = mutable.HashMap.empty

    def removeDecisionLiterals(decisionLiterals: mutable.HashSet[Literal]): Unit = {
      decisions --= decisionLiterals

      def valid(node: CRProofNode): Boolean = {
        mem.get(node) match {
          case Some(isValid) =>
            isValid
          case None =>
            val isValid: Boolean = node match {
              case Decision(literal) =>
                decisions.contains(literal)
              case Axiom(_) =>
                true
              case ConflictDrivenClauseLearning(_) =>
                true
              case UnitPropagationResolution(left, right, _, _, _) =>
                left.forall(valid) && valid(right)
            }
            mem(node) = isValid
            isValid
        }
      }

      for (literal <- reverseImplication.keys) {
        val reverseId = reverseImplication(literal)
        bufferNodes(reverseId) = bufferNodes(reverseId).filter(valid)
      }
      val nonValidClauses: Seq[Clause] = reverseImplication
        .filter(cl => bufferNodes(cl._2).isEmpty)
        .keys
        .toSeq
      val nonValidLiterals: Seq[Literal] = nonValidClauses
        .filter(_.isUnit)
        .map(_.literal)
      provedLiterals --= nonValidLiterals
      literals --= nonValidLiterals
      unifiableUnitsBuff.foreach(_ --= nonValidLiterals)
    }

    def getAllConflictDecisions(node: CRProofNode, acc: mutable.Set[Literal]): Unit =
      mem.get(node) match {
        case Some(true) =>
        case None =>
          mem.update(node, true)
          node match {
            case Decision(literal) =>
              acc += literal
            case _@Conflict(left, right) =>
              getAllConflictDecisions(left, acc)
              getAllConflictDecisions(right, acc)
            case UnitPropagationResolution(left, right, _, _, _) =>
              left.foreach(getAllConflictDecisions(_, acc))
              getAllConflictDecisions(right, acc)
            case ConflictDrivenClauseLearning(_) =>
            case Axiom(_) =>
          }
      }

    def addCDCLClauses(newClauses: Set[CRProofNode]): Unit = {
      cdclClauses ++= newClauses
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

      newClauses.foreach(node =>
        addNode(node.conclusion, node))

      addProvedLiterals(newClauses.toSeq.map(_.conclusion).filter(_.isUnit).map(_.literal))
    }

    def updateVSIDS(clauses: Seq[Clause]): Unit = {
      literals.foreach(lit => activity.update(lit, activity.getOrElseUpdate(lit, 0) * DECAY_FACTOR))
      clauses
        .foreach { clause =>
          val sz: Double = clause.literals.size
          clause.literals.foreach { lit =>
            activity.update(!lit, activity.getOrElseUpdate(!lit, 0) + BUMP / sz)
          }
        }
    }

    // already proved literals
    addProvedLiterals(cnf.clauses.filter(_.isUnit).map(_.literal))

    cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))

    updateVSIDS(cnf.clauses)

    var cntWithoutDecisions = 0

    while (true) {
//      println(s"size = ${unifiableUnitsBuff.map(l => l.size).sum}")
//      println(s"litSize = ${literals.size}")
      //      println("new iteration")
      val propagatedLiterals = mutable.Set.empty[Literal]
      for (clause <- nonUnitClauses)
        if (!clause.literals.exists(provedLiterals.contains)) {
          // if clause has not already proven
          resolve(clause, propagatedLiterals)
        }

      val wasPair: mutable.HashSet[(Clause, Clause)] = mutable.HashSet.empty
      addProvedLiterals(propagatedLiterals.toSeq)
      println(provedLiterals.size)

      // find clauses of kind `A & !B` where there is some unification for {A = B}
      val CDCLClauses = mutable.Set.empty[CRProofNode]
      provedLiterals
        .filter(l => unifiableUnitsBuff(unifiableUnitsIds(l)).nonEmpty)
        .foreach {
          conflictLiteral =>
            for {
              otherLiteral <- unifiableUnitsBuff(unifiableUnitsIds(conflictLiteral))
              conflictNode <- bufferNodes(reverseImplication(conflictLiteral))
              otherNode <- bufferNodes(reverseImplication(otherLiteral))
              conflict = Conflict(conflictNode, otherNode)
            } {
              if (!wasPair.contains((conflictNode.conclusion, otherNode.conclusion))) {
                wasPair.add((conflictNode.conclusion, otherNode.conclusion))
                val cdclNode = ConflictDrivenClauseLearning(conflict)
                val newClause = cdclNode.conclusion
                if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
                CDCLClauses += cdclNode
              }
            }
        }

      if (CDCLClauses.nonEmpty) {
        println(s"found ${CDCLClauses.size} conflicts")
        updateVSIDS(CDCLClauses.map(_.conclusion).toSeq)
        val acc: mutable.HashSet[Literal] = mutable.HashSet.empty
        mem.clear()
        CDCLClauses.foreach {
          case ConflictDrivenClauseLearning(cl) => getAllConflictDecisions(cl, acc)
//            val decisionLiterals: mutable.HashSet[Literal] = mutable.HashSet.empty[Literal]
//            getAllConflictDecisions(cl, decisionLiterals)
//            if (decisionLiterals.nonEmpty) {
//              acc.add(decisionLiterals.minBy[Double](lit => activity.getOrElse(lit, 0)))
//            }
        }
        mem.clear()
        removeDecisionLiterals(acc)
        addCDCLClauses(CDCLClauses.toSet)
//        reset(CDCLClauses.toSet)
      } else if (propagatedLiterals.isEmpty || (cntWithoutDecisions == MAX_CNT_WITHOUT_DECISIONS)) {
        cntWithoutDecisions = 0
        val available = rnd.shuffle((literals -- provedLiterals -- provedLiterals.map(!_)).toSeq)
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          val decisionLiteral = {
            if (rnd.nextInt(100) >= 20) {
              available.maxBy(l => activity(l))
            } else {
              available.head
            }
          }
          activity.update(decisionLiteral, 0)
//          println(s"max activity is ${activity(decisionLiteral)} for `${decisionLiteral}`")
          addProvedLiterals(Seq(decisionLiteral))
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiterals(mutable.HashSet(!decisionLiteral))
          }
          addNode(decisionLiteral, Decision(decisionLiteral))
        }
      } else if (cnf.clauses.forall(clause => clause.literals.exists(provedLiterals.contains))) {
        val literals = provedLiterals ++ decisions
        val trueLiterals = literals.filterNot(_.negated).map(_.unit).toSet
        val falseLiterals = literals.filter(_.negated).map(_.unit).map(x => Neg(x)).toSet
        return Satisfiable(Some(new Assignment(trueLiterals ++ falseLiterals)))
      } else {
        cntWithoutDecisions += 1
        // TODO: to do something in that case...
      }
    }

    Error // this line is unreachable.
  }

  // scalastyle:on
}
