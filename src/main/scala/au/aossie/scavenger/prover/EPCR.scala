package au.aossie.scavenger.prover

import au.aossie.scavenger.expression._
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.preprocessing.{AddEqualityReasoningAxioms, ClausesTo3CNF}
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.prover.inferences.InferenceRules
import au.aossie.scavenger.prover.util.{DecisionMaker, UnificationSearcher}
import au.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
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

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    implicit val logger = Logger(LoggerFactory.getLogger("prover"))

    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(InitialStatement(Clause.empty))))
    }

    val initialClauses = cnf.clauses.to[ListBuffer]

    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Generates new decision
      */
    val decisionMaker: DecisionMaker = new DecisionMaker(initialBump, decayFactor, maxActivity, randomDecisionsPercent)

    /**
      * Support unification data structure
      */
    val unificationSearcher = new UnificationSearcher(mutable.HashSet.empty)

    /**
      * CDCL and UnitPropagation inference rules here
      */
    val inferenceRules = new InferenceRules(unificationSearcher, decisionMaker, decisions, withSetOfSupport)

    val predicates = cnf.predicates
    val isEqualityReasoning = predicates.contains((new Sym("=") with Infix, 2))
    if (isEqualityReasoning) {
      logger.info("Equality reasoning problem")
      AddEqualityReasoningAxioms.add(initialClauses)
    }

//    ClausesTo3CNF.to3CNF(initialClauses)

    unificationSearcher.addNewClauses(initialClauses.filterNot(_.isUnit))

    /**
      * Memorization for getAllConflictDecisions method.
      */
    val memGetConflictDecisions: mutable.HashSet[Clause] = mutable.HashSet.empty

    def reset(): Unit = {
      logger.debug("RESET")
      decisions.clear()

//      nodesByClause.clear()
//
//      initialClauses.foreach(clause => addNode(clause, InitialStatement(clause)))
//      cdclClauses.foreach(clauseNode => addNode(clauseNode._1, clauseNode._2))
//
//      unificationSearcher.clearUnifiableUnits()
//
//      provedLiterals.clear()
//      addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
//      addProvedLiterals(cdclClauses.toSeq.map(_._1).filter(_.isUnit).map(_.literal))
//      logger.debug(s"provedLiterals.size = ${provedLiterals.size}")
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

    inferenceRules.addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
    initialClauses.foreach(clause => inferenceRules.addNode(clause, InitialStatement(clause)))

    var cntWithoutDecisions = 0

    while (true) {
      logger.debug(s"new iteration:  provedLiterals(${inferenceRules.provedLiterals.size})")
      val propagatedLiterals = mutable.Set.empty[Literal]
      unificationSearcher.clausesForPropagation(inferenceRules.provedLiterals).foreach( clause =>
        inferenceRules.resolveUnitPropagations(clause, propagatedLiterals)
      )


      logger.debug(s"propagated ${propagatedLiterals.size}")
      inferenceRules.addProvedLiterals(propagatedLiterals.toSeq)

      // find clauses of kind `A & !B` where there is some unification for {A = B}
      val CDCLClauses = mutable.ListBuffer.empty[CRProofNode]
      inferenceRules.CDCLResolve(CDCLClauses) match {
        case None =>
        case Some(problemStatus) =>
          return problemStatus
      }

      if (CDCLClauses.nonEmpty) {
        val acc: mutable.HashSet[Literal] = mutable.HashSet.empty
        memGetConflictDecisions.clear()
        CDCLClauses.foreach {
          case ConflictDrivenClauseLearning(cl) =>
            getAllConflictDecisions(cl, acc)
        }

        inferenceRules.removeDecisionLiteralsFromInferences(acc)
        inferenceRules.addCDCLClauses(CDCLClauses)
      } else if (propagatedLiterals.isEmpty ||
        (cntWithoutDecisions >= maxCountWithoutDecisions)) {
        cntWithoutDecisions = 0
        val available = inferenceRules.available
        if (available.isEmpty) {
          reset()
        } else {
          val decisionLiteral = decisionMaker.makeDecision(available.toSeq)
          inferenceRules.addNode(decisionLiteral.toClause, Decision(decisionLiteral))
          inferenceRules.addProvedLiterals(Seq(decisionLiteral))
//          val decisionActivity = getActivity(decisionLiteral)
//          println(s"NEW DECISION: $decisionLiteral, activity: $decisionActivity")
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            inferenceRules.removeDecisionLiteralsFromInferences(mutable.HashSet(!decisionLiteral))
          }
        }
      } else if (initialClauses.forall(clause => clause.literals.exists(inferenceRules.provedLiterals.contains))) {
        val literals = inferenceRules.provedLiterals ++ decisions
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
