package au.aossie.scavenger.prover.inferences

import au.aossie.scavenger.expression.{AppRec, E, Sym, Var}
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr.{CRProofNode, Conflict, ConflictDrivenClauseLearning, Decision, InitialStatement, UnitPropagationResolution}
import au.aossie.scavenger.prover.{ProblemStatus, Unsatisfiable, renameVars, unifyWithRename}
import au.aossie.scavenger.prover.heuristic.{DecisionMaker, UnificationSearcher}
import au.aossie.scavenger.structure.immutable.{Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import au.aossie.scavenger.unification.{MartelliMontanari => unify}
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}

import scala.collection.immutable.HashSet

/**
  * Created by podtelkin on 18.07.17.
  */
class InferenceRules(initialClauses: ListBuffer[Clause],
                     decisionMaker: DecisionMaker,
                     decisions: mutable.Set[Literal],
                     withSetOfSupport: Boolean)(implicit rnd: Random) extends UnificationSearcher {
  // FIXME: Bad practice to use predefined name(could be collision)
  val VARIABLE_NAME: String = "___VARIABLE___"

  /**
    * For every proved clause we need to know all possible proofNodes, because premises affect on result
    */
  val proofNodesByClause: mutable.Map[Clause, ListBuffer[CRProofNode]] = mutable.Map.empty

  /**
    * provedLiterals generated from initial clauses, cdcl clauses, current state of decisions and UnitPropagations
    */
  val provedLiterals: mutable.Set[Literal] = mutable.Set.empty

  /**
    * For every CDCL clause we need only one proofNode
    */

  val cdclNodes: mutable.Map[Clause, CRProofNode] = mutable.Map.empty

//  val unificationSearcher = new UnificationSearcher(mutable.HashSet.empty)




  reset()


  def available = nonUnitClauses.filterNot(_.literals.exists(provedLiterals.contains)).flatMap(_.literals)

  def reset(): Unit = {
    clearUnifiableUnits()

    proofNodesByClause.clear()

    cdclNodes.foreach { case (clause, node) =>
      proofNodesByClause.update(clause, ListBuffer(node))}
    initialClauses.foreach { clause =>
      proofNodesByClause.update(clause, ListBuffer(InitialStatement(clause)))
    }

    provedLiterals.clear()
    provedLiterals ++= cdclNodes.keys.filter(_.isUnit).map(_.literal)
    provedLiterals ++= initialClauses.filter(_.isUnit).map(_.literal)

    addNewClauses(cdclNodes.keys.filterNot(_.isUnit).toSeq)
    addNewClauses(initialClauses.filterNot(_.isUnit))

    updateUnifiableUnits(provedLiterals.toSeq)
  }

  def propagateAllClauses(): mutable.HashSet[Literal] = {
    val propagatedLiterals = mutable.HashSet.empty[Literal]
    clausesForPropagation(provedLiterals).foreach( clause =>
      resolveUnitPropagations(clause, propagatedLiterals)
    )
    propagatedLiterals
  }

  def addProvedLiterals(literals: Seq[Literal]) = {
    val newLiterals = literals.filterNot(provedLiterals.contains)
    provedLiterals ++= literals
    updateUnifiableUnits(newLiterals)
  }

  def addNode(clause: Clause, node: CRProofNode): Unit = {
    proofNodesByClause.getOrElseUpdate(clause, ListBuffer.empty).append(node)
  }

  def addCDCLClauses(nodes: Seq[CRProofNode]): Unit = {
    val newClauses = nodes.filterNot(node => cdclNodes.contains(node.conclusion))
    cdclNodes ++= newClauses.map(node => (node.conclusion, node))

    decisionMaker.update(newClauses.map(_.conclusion))
    addNewClauses(newClauses.map(_.conclusion).filterNot(_.isUnit))

    newClauses.foreach(node =>
      addNode(node.conclusion, node))

    addProvedLiterals(newClauses.map(_.conclusion).filter(_.isUnit).map(_.literal))
  }

  def addNewCDCLClauses(CDCLClauses: mutable.ListBuffer[CRProofNode]): Unit = {
    val acc: Set[Literal] = CDCLClauses.foldLeft(HashSet.empty[Literal]) {
      case (acc: HashSet[Literal], ConflictDrivenClauseLearning(conflict)) =>
        acc ++ conflict.decisionsWithoutSubst
    }

    removeConflictPremises(acc)
    addCDCLClauses(CDCLClauses)
  }

  def getBucketByExpr(expr: E): String = expr match {
    // NOTE: Var should be before Sym !!!
    case Var(_) =>
      VARIABLE_NAME
    case Sym(name) =>
      name
    case AppRec(Sym(name), _) =>
      name
  }


  def CDCLResolve(CDCLClauses: ListBuffer[CRProofNode]): Option[ProblemStatus] = {
    val provedLiteralBuckets: mutable.Map[String, ListBuffer[Literal]] = mutable.Map.empty
    for (literal <- provedLiterals.toSeq) {
      val bucketName = getBucketByExpr(literal.unit)

      val candidateLiterals = {
        if (bucketName == VARIABLE_NAME) {
          provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal])
        } else {
          provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal]) ++
            provedLiteralBuckets.getOrElse(VARIABLE_NAME, ListBuffer.empty[Literal])
        }
      }

      for {
        otherLiteral <- candidateLiterals if (literal.polarity != otherLiteral.polarity) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined
        conflictNode <- proofNodesByClause(literal.toClause)
        otherNode <- proofNodesByClause(otherLiteral.toClause)
        conflict = Conflict(conflictNode, otherNode)
      } {
        val cdclNode = ConflictDrivenClauseLearning(conflict)
        val newClause = cdclNode.conclusion
        if (newClause == Clause.empty) return Some(Unsatisfiable(Some(Proof(conflict))))
        CDCLClauses += cdclNode
      }
      provedLiteralBuckets.getOrElseUpdate(VARIABLE_NAME, ListBuffer.empty).append(literal)
      if (bucketName != VARIABLE_NAME) {
        provedLiteralBuckets.getOrElseUpdate(bucketName, ListBuffer.empty).append(literal)
      }
    }
    None
  }

  def removeConflictPremises(decisionLiterals: Set[Literal]): Unit = {
    decisions --= decisionLiterals

    val memIsValid: mutable.HashMap[Clause, Boolean] = mutable.HashMap.empty

    def isValidCheck(node: CRProofNode): Boolean = {
      memIsValid.get(node.conclusion) match {
        case Some(isValid) =>
          isValid
        case None =>
          val isValid: Boolean = node match {
            case Decision(literal) =>
              decisions.contains(literal)
            case InitialStatement(_) =>
              true
            case ConflictDrivenClauseLearning(_) =>
              true
            case UnitPropagationResolution(left, right, _, _, _) =>
              left.forall(isValidCheck) && isValidCheck(right)
            case Conflict(left, right) =>
              isValidCheck(left) && isValidCheck(right)
          }
          memIsValid.put(node.conclusion, isValid)
          isValid
      }
    }


    provedLiterals.foreach { literal =>
      proofNodesByClause(literal.toClause) = proofNodesByClause(literal.toClause).filter { cRProofNode: CRProofNode =>
        cRProofNode match {
          case ConflictDrivenClauseLearning(conflict) => isValidCheck(conflict)
          case other => isValidCheck(other)
        }
      }
    }
    val nonValidLiterals: Seq[Literal] = provedLiterals.toSeq.filter(literal => proofNodesByClause(literal.toClause).isEmpty)
    provedLiterals --= nonValidLiterals

    removeNonValidLiterals(nonValidLiterals)
  }


  def resolveUnitPropagations(clause: Clause, result: mutable.Set[Literal]): Unit = {
    for (clauseNode <- proofNodesByClause(clause)) {

      // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
      val shuffledLiterals = clause.literals

      val unifyCandidates = shuffledLiterals.map(literal => getUnifiers(literal))
      for (conclusionId <- unifyCandidates.indices) {
        // TODO: Think about other cut offs.
        if (unifyCandidates.indices.forall(id => id == conclusionId || unifyCandidates(id).nonEmpty)) {

          val unifiers: Seq[Seq[Literal]] = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
          val literals: Seq[Literal] = shuffledLiterals.take(conclusionId) ++ shuffledLiterals.drop(conclusionId + 1)

          def newPropagation(chosenUnifiers: mutable.Seq[Literal],
                             subs: mutable.Seq[Substitution],
                             globalSubst: Substitution,
                             usedVars: Set[Var]): Unit = {
            val unifierNodes = chosenUnifiers.map(l => rnd.shuffle(proofNodesByClause(l.toClause)).head)
            if (!withSetOfSupport || unifierNodes.exists(!_.isAxiom) || !clauseNode.isAxiom) {
              val curSubst = renameVars(shuffledLiterals(conclusionId).unit, usedVars)
              val unitPropagationNode =
                UnitPropagationResolution(
                  unifierNodes,
                  clauseNode,
                  shuffledLiterals(conclusionId),
                  literals,
                  subs,
                  globalSubst
                )
              val newLiteral = unitPropagationNode.conclusion.literal
              if (!result.contains(newLiteral) && !provedLiterals.contains(newLiteral)) {
                addNode(newLiteral.toClause, unitPropagationNode)
                result += newLiteral
              }
            }
          }

          /**
            * Recursively generates UnitPropagationResolution and verifies unification on each step.
            *
            * @param chosenUnifiers    chosen literals
            * @param subs              substitutions for literals to ensure that variables in different literals are different
            * @param literalsWithSubst literals with unique variables
            * @param globalSubst       same global MGU for all literals
            * @param cur               number of chosen literals
            */
          def go(chosenUnifiers: mutable.Seq[Literal],
                 unifierWithSub: Seq[E],
                 subs: mutable.Seq[Substitution],
                 literalsWithSubst: Seq[Literal],
                 globalSubst: Substitution,
                 usedVars: Set[Var],
                 cur: Int): Unit = {

            if (cur == unifiers.size) {
              newPropagation(chosenUnifiers, subs, globalSubst, usedVars)
              return
            }

            // NOTE: Looking at all possible unifications turns to large complexity of this part of resolving
            // TODO: Think about to check only random K unifiers
            val candidates = {
              //                rnd.shuffle(unifiers(cur)).take(maxCountCandidates)
              unifiers(cur)
            }
            for (curUni <- candidates) {
              val substitution = renameVars(curUni.unit, usedVars)
              val newSubs = subs :+ substitution(globalSubst)
              val leftWithSubst = globalSubst(substitution(curUni.unit))
              val newUnifierWithSubst = unifierWithSub :+ leftWithSubst

              // NOTE: Very dangerous to call pure unify method
              val unificationSubst = unify(leftWithSubst, literalsWithSubst(cur).unit)
              unificationSubst match {
                case Some(uniSubst) =>
                  go(chosenUnifiers :+ curUni,
                    newUnifierWithSubst.map(uniSubst(_)),
                    newSubs.map(_ (uniSubst)),
                    literalsWithSubst.map(uniSubst(_)),
                    globalSubst(uniSubst),
                    usedVars ++ leftWithSubst.variables,
                    cur + 1
                  )
                case None =>
              }
            }
          }

          go(mutable.Seq.empty,
            Seq.empty,
            mutable.Seq.empty,
            literals,
            Substitution.empty,
            literals.map(_.unit.variables).reduce {
              _ ++ _
            } toSet,
            0)
        }
      }
    }
  }

}
