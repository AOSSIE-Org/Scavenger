package org.aossie.scavenger.prover

import java.util.concurrent.TimeUnit

import org.aossie.scavenger.expression._
import org.aossie.scavenger.model.Assignment
import org.aossie.scavenger.preprocessing.AddEqualityReasoningAxioms
import org.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import org.aossie.scavenger.prover.inferences.InferenceRules
import org.aossie.scavenger.prover.heuristic.DecisionMaker
import org.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import com.typesafe.scalalogging.Logger

import scala.concurrent.duration.Duration


/**
  * @author Daniyar Itegulov
  */
class EPCR(maxCountCandidates: Int = 1000,
           maxCountWithoutDecisions: Int = 10,
           initialBump: Double = 1.0,
           decayFactor: Double = 0.99,
           maxActivity: Double = 1e10,
           randomDecisionsPercent: Double = 5,
           withSetOfSupport: Boolean = true) extends Prover {
  //   TODO: Do research about these constants

  //   TODO: Think about every usage of randomness
  private implicit val rnd: Random = new Random(107)

  private val logger = Logger[this.type]

  // scalastyle:off
  override def prove(cnf: CNF, timeout: Duration = Duration.Inf): ProblemStatus = {
    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(InitialStatement(Clause.empty))))
    }
    
    val startTime = System.currentTimeMillis

    val initialClauses = cnf.clauses.to[ListBuffer]

    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Generates new decision
      */
    val decisionMaker: DecisionMaker = new DecisionMaker(initialBump, decayFactor, maxActivity, randomDecisionsPercent)

    val predicates = cnf.predicates
    val isEqualityReasoning = predicates.contains((new Sym("=") with Infix, 2))
    if (isEqualityReasoning) {
      logger.info("Equality reasoning problem")
      AddEqualityReasoningAxioms.add(initialClauses)
    }

//    ClausesTo3CNF.to3CNF(initialClauses)

    /**
      * CDCL and UnitPropagation inference rules here
      */
    val inferenceRules = new InferenceRules(initialClauses, decisionMaker, decisions, withSetOfSupport)

    def reset(): Unit = {
      logger.debug("RESET")

      decisions.clear()
      decisionMaker.reset()
      inferenceRules.reset()
    }

    inferenceRules.reset()
    var cntWithoutDecisions = 0

    while (true) {
      logger.debug(s"new iteration:  provedLiterals(${inferenceRules.provedLiterals.size})")
      val propagatedLiterals = inferenceRules.propagateAllClauses()


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
        inferenceRules.addNewCDCLClauses(CDCLClauses)
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
          println(decisionLiteral)
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            inferenceRules.removeConflictPremises(Set(!decisionLiteral))
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

      val endTime = System.currentTimeMillis
      val time = Duration.apply(endTime - startTime, TimeUnit.MILLISECONDS)
      if (time >= timeout) {
        return Timeout
      }
    }

    Error // this line is unreachable.
  }

  // scalastyle:on
}

object EPCR extends EPCR(
  maxCountCandidates = 100,
  maxCountWithoutDecisions = 5,
  initialBump = 1.0,
  decayFactor = 0.99,
  maxActivity = 1e10,
  randomDecisionsPercent = 5,
  withSetOfSupport = true)
