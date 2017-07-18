package au.aossie.scavenger.prover

import au.aossie.scavenger.expression._
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.preprocessing.{AddEqualityReasoningAxioms, ClausesTo3CNF}
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.prover.util.{DecisionMaker, UnificationSearcher}
import au.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import au.aossie.scavenger.unification.{MartelliMontanari => unify}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory


/**
  * @author Daniyar Itegulov
  */
class EPCR(maxCountCandidates: Int = 1000,
           maxCountWithoutDecisions: Int = 10,
           maxProvedLiteralsSize: Int = 10000,
           initialBump: Double = 1.0,
           decayFactor: Double = 0.99,
           maxActivity: Double = 1e10,
           randomDecisionsPercent: Double = 5,
           withSetOfSupport: Boolean = true) extends Prover {
  //   TODO: Do research about these constants

  //   TODO: Think about every usage of randomness
  implicit val rnd = new Random(107)

  // FIXME: Bad practice to use predefined name(could be collision)
  val VARIABLE_NAME: String = "___VARIABLE___"

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    implicit val logger = Logger(LoggerFactory.getLogger("prover"))

    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(InitialStatement(Clause.empty))))
    }

    val initialClauses = cnf.clauses.to[ListBuffer]

    val decisionMaker: DecisionMaker = new DecisionMaker(initialBump, decayFactor, maxActivity, randomDecisionsPercent)
    val unificationSearcher = new UnificationSearcher(mutable.HashSet.empty)

    val predicates = cnf.predicates
    val isEqualityReasoning = predicates.contains((new Sym("=") with Infix, 2))
    if (isEqualityReasoning) {
      logger.info("Equality reasoning problem")
      AddEqualityReasoningAxioms.add(initialClauses)
    }

//    ClausesTo3CNF.to3CNF(initialClauses)

    unificationSearcher.addNewClauses(initialClauses.filterNot(_.isUnit))

    /**
      * Mutable set of proved literals initialized with the input CNF's unit clauses.
      */
    val provedLiterals: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Mutable map showing all possible proofs for every proved clause.
      */
    val nodesByClause: mutable.Map[Clause, ListBuffer[CRProofNode]] = mutable.Map.empty

    /**
      * Set of clauses proved using CDCL rule.
      */
    val cdclClauses: mutable.Map[Clause, CRProofNode] = mutable.Map.empty

    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Memorization for isValidCheck method.
      */
    val memIsValid: mutable.HashMap[Clause, Boolean] = mutable.HashMap.empty

    /**
      * Memorization for getAllConflictDecisions method.
      */
    val memGetConflictDecisions: mutable.HashSet[Clause] = mutable.HashSet.empty

    def addProvedLiterals(literals: Seq[Literal]) = {
      val newLiterals = literals.filterNot(provedLiterals.contains)
      provedLiterals ++= literals
      unificationSearcher.updateUnifiableUnits(newLiterals)
    }

    def addNode(clause: Clause, node: CRProofNode): Unit = {
      nodesByClause.getOrElseUpdate(clause, ListBuffer.empty).append(node)
    }

    def resolveUnitPropagations(clause: Clause, result: mutable.Set[Literal]): Unit = {
      for (clauseNode <- nodesByClause(clause)) {

        // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
        val shuffledLiterals = clause.literals

        val unifyCandidates = shuffledLiterals.map(literal => unificationSearcher.getUnifiers(literal))
        for (conclusionId <- unifyCandidates.indices) {
          // TODO: Think about other cut offs.
          if (unifyCandidates.indices.forall(id => id == conclusionId || unifyCandidates(id).nonEmpty)) {

            val unifiers: Seq[Seq[Literal]] = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
            val literals: Seq[Literal] = shuffledLiterals.take(conclusionId) ++ shuffledLiterals.drop(conclusionId + 1)

            def newPropagation(chosenUnifiers: mutable.Seq[Literal],
                               subs: mutable.Seq[Substitution],
                               globalSubst: Substitution,
                               usedVars: mutable.Set[Var]): Unit = {
              val unifierNodes = chosenUnifiers.map(l => rnd.shuffle(nodesByClause(l)).head)
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
                   usedVars: mutable.Set[Var],
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
              mutable.Set[Var](literals.map(_.unit.variables).reduce {
                _ ++ _
              }: _*),
              0)
          }
        }
      }
    }

    def reset(): Unit = {
      logger.debug("RESET")
      decisions.clear()

      nodesByClause.clear()

      initialClauses.foreach(clause => addNode(clause, InitialStatement(clause)))
      cdclClauses.foreach(clauseNode => addNode(clauseNode._1, clauseNode._2))

      unificationSearcher.clearUnifiableUnits()

      provedLiterals.clear()
      addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
      addProvedLiterals(cdclClauses.toSeq.map(_._1).filter(_.isUnit).map(_.literal))
      logger.debug(s"provedLiterals.size = ${provedLiterals.size}")
    }

    def removeDecisionLiterals(decisionLiterals: mutable.HashSet[Literal]): Unit = {
      decisions --= decisionLiterals

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
        nodesByClause(literal.toClause) = nodesByClause(literal.toClause).filter { cRProofNode: CRProofNode =>
          cRProofNode match {
            case ConflictDrivenClauseLearning(conflict) => isValidCheck(conflict)
            case other => isValidCheck(other)
          }
        }
      }
      val nonValidLiterals: Seq[Literal] = provedLiterals.toSeq.filter(literal => nodesByClause(literal.toClause).isEmpty)
      provedLiterals --= nonValidLiterals

      unificationSearcher.removeNonValidLiterals(nonValidLiterals)
    }

    def getAllConflictDecisions(node: CRProofNode, acc: mutable.Set[Literal]): Unit =
      if (!memGetConflictDecisions.contains(node.conclusion)) {
        memGetConflictDecisions.add(node.conclusion)
        node match {
          case Decision(literal) =>
            acc += literal
          case Conflict(left, right) =>
            getAllConflictDecisions(left, acc)
            getAllConflictDecisions(right, acc)
          case UnitPropagationResolution(left, right, _, _, _) =>
            left.foreach(getAllConflictDecisions(_, acc))
            getAllConflictDecisions(right, acc)
          case ConflictDrivenClauseLearning(_) =>
          case InitialStatement(_) =>
        }
      }

    def addCDCLClauses(nodes: Seq[CRProofNode]): Unit = {
      val newClauses = nodes.filterNot(node => cdclClauses.contains(node.conclusion))
      cdclClauses ++= newClauses.map(node => (node.conclusion, node))

      unificationSearcher.addNewClauses(newClauses.map(_.conclusion).filterNot(_.isUnit))

      decisionMaker.update(newClauses.map(_.conclusion))

      newClauses.foreach(node =>
        addNode(node.conclusion, node))

      addProvedLiterals(newClauses.map(_.conclusion).filter(_.isUnit).map(_.literal))

      logger.debug(s"added ${newClauses.size} new conflicts")
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


    addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
    initialClauses.foreach(clause => addNode(clause, InitialStatement(clause)))

    var cntWithoutDecisions = 0

    while (true) {
      logger.debug(s"new iteration:  provedLiterals(${provedLiterals.size})")
      val propagatedLiterals = mutable.Set.empty[Literal]
      unificationSearcher.clausesForPropagation(provedLiterals).foreach( clause =>
        resolveUnitPropagations(clause, propagatedLiterals)
      )


      logger.debug(s"propagated ${propagatedLiterals.size}")
      addProvedLiterals(propagatedLiterals.toSeq)

      // find clauses of kind `A & !B` where there is some unification for {A = B}
      val CDCLClauses = mutable.ListBuffer.empty[CRProofNode]

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
          conflictNode <- nodesByClause(literal.toClause)
          otherNode <- nodesByClause(otherLiteral.toClause)
          conflict = Conflict(conflictNode, otherNode)
        } {
          val cdclNode = ConflictDrivenClauseLearning(conflict)
          val newClause = cdclNode.conclusion
          if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
          CDCLClauses += cdclNode
        }
        provedLiteralBuckets.getOrElseUpdate(VARIABLE_NAME, ListBuffer.empty).append(literal)
        if (bucketName != VARIABLE_NAME) {
          provedLiteralBuckets.getOrElseUpdate(bucketName, ListBuffer.empty).append(literal)
        }
      }
      if (CDCLClauses.nonEmpty) {
        val acc: mutable.HashSet[Literal] = mutable.HashSet.empty
        memGetConflictDecisions.clear()
        CDCLClauses.foreach {
          case ConflictDrivenClauseLearning(cl) =>
            getAllConflictDecisions(cl, acc)
        }

        memIsValid.clear()
        removeDecisionLiterals(acc)
        addCDCLClauses(CDCLClauses)
      } else if (propagatedLiterals.isEmpty ||
        (cntWithoutDecisions >= maxCountWithoutDecisions) ||
        (provedLiterals.size > maxProvedLiteralsSize)) {
        cntWithoutDecisions = 0
        val available =
          unificationSearcher.nonUnitClauses.filterNot(
            clause =>
              clause.literals
                .exists(provedLiterals.contains))
            .flatMap(_.literals)(collection.breakOut).toSet -- provedLiterals.map(!_)
        if (available.isEmpty) {
          reset()
        } else {
          val decisionLiteral = decisionMaker.makeDecision(available.toSeq)
          addNode(decisionLiteral.toClause, Decision(decisionLiteral))
          addProvedLiterals(Seq(decisionLiteral))
//          val decisionActivity = getActivity(decisionLiteral)
//          println(s"NEW DECISION: $decisionLiteral, activity: $decisionActivity")
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiterals(mutable.HashSet(!decisionLiteral))
          }
        }
      } else if (initialClauses.forall(clause => clause.literals.exists(provedLiterals.contains))) {
        val literals = provedLiterals ++ decisions
        val (positiveLiterals, negativeLiterals) = literals.partition(_.polarity)
        return Satisfiable(Some(new Assignment(positiveLiterals.map(_.unit).toSet ++ negativeLiterals.map(_.unit).toSet)))
      } else {
        // TODO: think about that case...
        cntWithoutDecisions += 1
      }
    }

    Error // this line is unreachable.
  }

  // scalastyle:on
}

object EPCR extends EPCR(
  maxCountCandidates = 100,
  maxCountWithoutDecisions = 5,
  maxProvedLiteralsSize = 10000,
  initialBump = 1.0,
  decayFactor = 0.99,
  maxActivity = 1e10,
  randomDecisionsPercent = 5,
  withSetOfSupport = true)
