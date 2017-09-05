package au.aossie.scavenger.prover.actors

import akka.actor.ActorSystem
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.{AppRec, E, Sym, Var}
import au.aossie.scavenger.proof.cr._
import au.aossie.scavenger.structure.immutable.{Clause, Literal}
import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.choosing.DecisionsMaker
import au.aossie.scavenger.unification.{Unificator, MartelliMontanari => unify}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random


/**
  * Every expertActor stores this data structure to store clauses and to derive new clauses.
  *
  * @param predicates Set of predicates supported by the actor.
  * @param withSetOfSupport Flag indicating to use "Set of support" strategy.
  * @param maxIterationsWithoutDecision Maximum number of iterations without making a new decision.
  * @param rnd Implicit structure to generate random numbers
  */
class ExpertData(predicates: Set[Sym], withSetOfSupport: Boolean, maxIterationsWithoutDecision: Int)(implicit rnd: Random) {

  val clauses: ListBuffer[ClauseInfo] = ListBuffer.empty
  val indexByClause: mutable.Map[Clause, Int] = mutable.Map.empty

  /**
    * Every node of the proof-graph can be uniquely identified by the current clause and the list of decisions
    * which will be used in ConflictResolution inference rule.
    * This Set memorizes all possible pairs of (currentClause, decisionsClause).
    */
  val memorizedClauses: mutable.Set[(Clause, Clause)] = mutable.Set.empty

  /**
    * Position in [[clauses]] for every derived unit clause(i. e. literal).
    */
  val provedLiterals: mutable.Map[Literal, Int] = mutable.Map.empty

  /**
    * Buffers new proved literals to process it in [[resolveCDCL]] phase.
    * Cleared in [[resolveCDCL]]
    */
  val lastPropagatedLiterals: mutable.ListBuffer[(Literal, CRProofNode)] = mutable.ListBuffer.empty

  val unificator: Unificator = new Unificator()
  val decisionsMaker = new DecisionsMaker(maxIterationsWithoutDecision)
  val propagatedLiteralsBuffer: ListBuffer[CRProofNode] = mutable.ListBuffer.empty[CRProofNode]

  def addNewClause(crProofNode: CRProofNode): Unit = {
    val expertClause = crProofNode.conclusion
    val clause = (expertClause, (crProofNode.nonExpertDecisions ++ crProofNode.decisions).toClause)
    if (!memorizedClauses.contains(clause)) {
      memorizedClauses.add(clause)
      if (expertClause.isUnit) {
        lastPropagatedLiterals.append((crProofNode.conclusion.literal, crProofNode))
      }

      val index = indexByClause.getOrElseUpdate(expertClause, clauses.size)
      if (index == clauses.size) {
        clauses += ClauseInfo(expertClause, ListBuffer(crProofNode))

        if (expertClause.isUnit) {
          unificator.addB(expertClause.literal)
          provedLiterals.update(expertClause.literal, clauses.size - 1)
        } else {
          expertClause.literals.foreach(unificator.addA)
        }
      } else {
        clauses(index).proofs.append(crProofNode)
      }
    }
  }

  def addInitialClauses(newClauses: Seq[Clause]): Unit = {
    newClauses.foreach { clause =>
      val initialStatement = new InitialStatement(clause)
      val node = Expertise(initialStatement, predicates)
      if (node.conclusion != Clause.empty) {
        addNewClause(node)
      }
    }
  }

  def addClauses(nodes: Seq[CRProofNode]): Unit = {
    nodes.foreach { globalNode =>
      val localNode = Expertise(globalNode, predicates)
      if (localNode.conclusion != Clause.empty) {
        addNewClause(localNode)
      }
    }
  }

  def resolveUnitPropagation(): Unit = {
    propagatedLiteralsBuffer.clear()
    clauses.collect { case clauseInfo@ClauseInfo(expertClause, proofs) if proofs.nonEmpty && !expertClause.isUnit => {
      val unifiers = expertClause.literals.map(
        literal =>
          unificator
            .getUnifications(literal)
            .filter(otherLiteral => indexByClause.get(otherLiteral) match {
              case Some(index) =>
                clauses(index).proofs.nonEmpty
              case None =>
                false
            } )
      )
      val conclusionCandidateIds = {
        val emptyLiterals = unifiers.zipWithIndex.filter(_._1.isEmpty)
        if (emptyLiterals.isEmpty) {
          unifiers.indices
        } else if (emptyLiterals.size == 1) {
          emptyLiterals.map(_._2)
        } else {
          Seq.empty
        }
      }

      conclusionCandidateIds.foreach(
        conclusionId =>
          resolveUnitPropagationByClause(clauseInfo, unifiers, conclusionId)
      )
    } }
    propagatedLiteralsBuffer.foreach(addNewClause)
  }

  def resolveUnitPropagationByClause(clauseInfo: ClauseInfo, unifiers: Seq[mutable.ListBuffer[Literal]], conclusionId: Int): Unit =
    clauseInfo match { case ClauseInfo(clause, proofs) => {
      val unifyCandidates = unifiers.take(conclusionId) ++ unifiers.drop(conclusionId + 1)
      val propagatingLiterals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)

      def generateNewPropagations(choosenUnifiers: Seq[Literal],
                                 substitutions: Seq[Substitution],
                                 globalSubstitution: Substitution,
                                 usedVariables: Set[Var]): Unit = {
        for (clauseNode <- proofs) {
          for (nodes <- combinations(choosenUnifiers.map(literal => clauses(indexByClause(literal)).proofs))) {
            if (!withSetOfSupport || nodes.exists(!_.isAxiom) || !clauseNode.isAxiom) {
              val unitPropagationNode = UnitPropagationResolution(
                nodes,
                clauseNode,
                clause.literals(conclusionId),
                propagatingLiterals,
                substitutions,
                globalSubstitution
              )
              // FIXME: should be arguments
              if ((unitPropagationNode.decisions.size <= 5) && (unitPropagationNode.conclusion.literal.unit.logicalSize <= 50)) {
                propagatedLiteralsBuffer.append(unitPropagationNode)
              }
            }
          }
        }
      }

      def rec(choosenUnifiers: Seq[Literal],
              choosenUnifiersWithSubst: Seq[Literal],
              substitutions: Seq[Substitution],
              usedVariables: Set[Var],
              globalSubstitution: Substitution,
              curId: Int): Unit = {
        if (curId < unifyCandidates.size) {
          val candidates = {
            // TODO: we could try to take some part of available unifiers
            unifyCandidates(curId)
          }
          candidates.foreach { literalCandidate =>
            val substitution = renameVars(literalCandidate.unit, usedVariables)
            val newSubstitutions = substitutions :+ substitution(globalSubstitution)
            val literalWithSubstitution = Literal(globalSubstitution(substitution(literalCandidate.unit)), literalCandidate.polarity)
            val newChoosenUnifiersWithSubst = choosenUnifiersWithSubst :+ literalWithSubstitution

            // NOTE: pure call of unify
            unify(literalWithSubstitution.unit, globalSubstitution(propagatingLiterals(curId).unit)) match {
              case Some(localSubstitution) =>
                rec(
                  choosenUnifiers :+ literalCandidate,
                  newChoosenUnifiersWithSubst.map(localSubstitution(_)),
                  newSubstitutions.map(sub => sub(localSubstitution)),
                  usedVariables ++ literalWithSubstitution.unit.variables,
                  globalSubstitution(localSubstitution),
                  curId + 1
                )
              case None =>
            }
          }

        } else {
          generateNewPropagations(choosenUnifiers, substitutions, globalSubstitution, usedVariables)
        }
      }

      rec(Seq.empty,
          Seq.empty,
          Seq.empty,
          clause.literals.map(_.unit.variables).reduce {
            _ ++ _
          }.toSet,
          Substitution.empty,
          0)
    } }

  def getBucketByExpr(expr: E): String = expr match {
    case AppRec(Sym(name), _) =>
      name
  }

  def resolveCDCL: Seq[CRProofNode] = {
    val provedLiteralBuckets: mutable.Map[String, ListBuffer[Literal]] = mutable.Map.empty
    val cdclNodes: mutable.ListBuffer[CRProofNode] = mutable.ListBuffer.empty
    val wasCDCL: mutable.Set[Clause] = mutable.Set.empty
    provedLiterals.foreach{
      case (literal, _) =>
        provedLiteralBuckets.getOrElseUpdate(getBucketByExpr(literal.unit), ListBuffer.empty).append(literal)
    }
    for ((literal, conflictNode) <- lastPropagatedLiterals) {
      val bucketName = getBucketByExpr(literal.unit)
      val candidateLiterals = provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal])

      for {
        otherLiteral <- candidateLiterals if (literal.polarity != otherLiteral.polarity) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined
        otherNode <- clauses(indexByClause(otherLiteral.toClause)).proofs
        conflict = Conflict(conflictNode, otherNode)
      } {
        val cdclNode = ConflictDrivenClauseLearning(conflict)
        if (!wasCDCL.contains(cdclNode.conclusion)) {
          wasCDCL.add(cdclNode.conclusion)
          cdclNodes += cdclNode
          if (cdclNode.conclusion == Clause.empty) {
            return cdclNodes
          }
        }
      }
    }


    lastPropagatedLiterals.clear()

    cdclNodes
  }

  def removeCDCLPremises(premises: Set[Literal]): Unit = {
    provedLiterals.foreach {
      case (_, index) =>
        val nonValidProofs = clauses(index).proofs.filter {
          case ConflictDrivenClauseLearning(conflict) =>
            conflict.decisions.exists(premises.contains)
          case node =>
            node.decisions.exists(premises.contains)
        }
        clauses(index).proofs --= nonValidProofs
    }

    val nonValidLiterals = provedLiterals.filter { case (_, index) => clauses(index).proofs.isEmpty }.keys
    provedLiterals --= nonValidLiterals
  }

  def makeDecision(wasNewCDCLs: Boolean): Unit = {
//    println("making decision")
    decisionsMaker.incCounter
    if (!wasNewCDCLs || decisionsMaker.counterExpired) {
      val available = clauses
        .filterNot(clauseInfo =>
          clauseInfo.proofs.isEmpty || clauseInfo.expertClause.literals.exists(provedLiterals.contains))
        .flatMap(_.expertClause.literals)
      if (available.nonEmpty) {
        val newDecision = decisionsMaker.makeDecision(available)
//        println(s"newDecision = $newDecision")
        addNewClause(Decision(newDecision))
      } else {
        // FIXME: think about when this case occures
//        println("WARNING!!! available is empty!!!")
      }
    }

  }
}

case class ClauseInfo(expertClause: Clause,
                      proofs: ListBuffer[CRProofNode]) /// proofNodes and the list of decisions for non-expert part of the clause
