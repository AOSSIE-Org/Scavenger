package org.aossie.scavenger.prover

import ammonite.ops.pwd
import org.aossie.scavenger.expression.{AppRec, Var}
import org.aossie.scavenger.model.Assignment
import org.aossie.scavenger.parser.TPTPCNFParser
import org.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import org.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}
import org.aossie.scavenger.unification.tools._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.util.Random

/**
  * @author Daniyar Itegulov
  */
object TDCR extends Prover {

  val rnd = new Random(132374)

  // scalastyle:off
  override def prove(cnf: CNF, timeout: Duration = Duration.Inf): ProblemStatus = {
    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(InitialStatement(Clause.empty))))
    }

    val propagatedLiterals = mutable.Set(cnf.clauses.filter(_.isUnit).map(_.literal): _*)
    val clauses = mutable.Set(cnf.clauses.filter(!_.isUnit): _*)
    val literals = mutable.Set(cnf.clauses.flatMap(_.literals): _*)
    val unifiableUnits = mutable.Map.empty[Literal, mutable.Set[Literal]]
    val reverseImplicationGraph = mutable.Map.empty[Clause, ArrayBuffer[CRProofNode]]
    val decisions = ArrayBuffer.empty[Literal]
    val conflictClauses = mutable.Set.empty[CRProofNode]
    val resolvedCache = mutable.Set.empty[(Clause, Int, Seq[Literal])]
    var termDepthThreshold = 1
    val maxInitialTermDepth = cnf.clauses.flatMap(_.literals).map(_.depth).max

    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
      literals ++= newLiterals
      propagatedLiterals ++= newLiterals
      for (literal <- literals) {
        val set = unifiableUnits.getOrElseUpdate(literal, mutable.Set.empty)
        for (newLiteral <- newLiterals) if (newLiteral.depth <= termDepthThreshold && newLiteral.polarity != literal.polarity) {
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
              if (!result.contains(newLiteral) && !propagatedLiterals.contains(newLiteral)) {
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
      val removedDecision = decisions.last
      decisions.remove(decisions.size - 1)
      
      conflictClauses ++= newClauses
      resolvedCache.clear()
      literals -= removedDecision
      literals ++= cnf.clauses.flatMap(_.literals) ++ conflictClauses.flatMap(_.conclusion.literals)
      var badLiterals = mutable.Set.empty[Literal]
      for (literal <- propagatedLiterals) {
        reverseImplicationGraph(literal) = reverseImplicationGraph(literal).filterNot {
          _.existsAmongAncestors {
            case decision: Decision if decision.literal == removedDecision => true
            case _ => false
          }
        }
        if (reverseImplicationGraph(literal).isEmpty) {
          badLiterals += literal
        }
      }
      propagatedLiterals --= badLiterals
      reverseImplicationGraph --= badLiterals.map(_.toClause)
      for ((_, toUnify) <- unifiableUnits) {
        toUnify --= badLiterals
      }
      newClauses.foreach { node =>
        reverseImplicationGraph.getOrElseUpdate(node.conclusion, ArrayBuffer.empty) += node
      }
      val newLiterals = newClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)
      if (newLiterals.nonEmpty) {
        propagatedLiterals ++= newLiterals
        updateUnifiableUnits(newLiterals.toList)
      }
      for (literal <- newClauses.flatMap(_.conclusion.literals)) {
        val set = unifiableUnits.getOrElseUpdate(literal, mutable.Set.empty)
        for (newLiteral <- propagatedLiterals) if (newLiteral.depth <= termDepthThreshold && newLiteral.polarity != literal.polarity) {
          unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
            case Some(_) => set += newLiteral
            case None    =>
          }
        }
      }
    }

    def fullReset(newClauses: Set[CRProofNode]): Unit = {
      resolvedCache.clear()
      conflictClauses ++= newClauses
      unifiableUnits.clear()
      literals.clear()
      literals ++= cnf.clauses.flatMap(_.literals) ++ conflictClauses.map(_.conclusion).flatMap(_.literals)
      propagatedLiterals.clear()
      decisions.clear()
      reverseImplicationGraph.clear()
      cnf.clauses.foreach(clause =>
        reverseImplicationGraph.getOrElseUpdate(clause, ArrayBuffer.empty) += InitialStatement(clause))
      decisions.foreach(decision =>
        reverseImplicationGraph.getOrElseUpdate(decision, ArrayBuffer.empty) += Decision(decision))
      conflictClauses.foreach(node =>
        reverseImplicationGraph.getOrElseUpdate(node.conclusion, ArrayBuffer.empty) += node)
      propagatedLiterals ++= cnf.clauses.filter(_.isUnit).map(_.literal)
      propagatedLiterals ++= conflictClauses.map(_.conclusion).filter(_.isUnit).map(_.literal)
      propagatedLiterals ++= decisions
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
          case InitialStatement(_) =>
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

    cnf.clauses.foreach(clause => reverseImplicationGraph(clause) = ArrayBuffer(InitialStatement(clause)))

    while (true) {
      val result = mutable.Set.empty[Literal]
      for (clause <- clauses) if (!clause.literals.exists(propagatedLiterals.contains)) {
        resolve(clause, result)
      }
      for (conflictClause <- conflictClauses) if (!conflictClause.conclusion.isUnit) {
        resolve(conflictClause.conclusion, result)
      }

      updateUnifiableUnits(result.toSeq)

      val CDCLClauses = mutable.Set.empty[CRProofNode]
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
          CDCLClauses += cdclNode
        }
      }

      if (CDCLClauses.nonEmpty) {
        reset(CDCLClauses.toSet)
      } else if (result.isEmpty) {
        if (decisions.size > 10) {
          fullReset(Set.empty)
        } else if (rnd.nextInt(100) > 10) {
          var chosen = false
          while (!chosen) {
            val provedPredicates = propagatedLiterals.filter { literal =>
              literal.unit match {
                case AppRec(_, args) => args.forall(_.isInstanceOf[Var]) && args.toSet.size == args.size
              }
            }.map(_.predicate)
            val (predicate, predicateArity) = rnd.shuffle((cnf.predicates.toSet -- provedPredicates).toList).head
            val polarity = rnd.nextBoolean()
            var blockingLiterals = propagatedLiterals.filter(literal => literal.polarity != polarity && literal.predicate._1 == predicate)
            var symbols = cnf.constantSymbols.toList
            
            
            val predicateArguments = for (i <- 0 until predicateArity) yield {
              val qVar = Var("X" + i)
              val symbol = rnd.shuffle(qVar :: symbols).head
              if (symbol.isInstanceOf[Var]) {
                symbols = qVar :: symbols
              } else {
                blockingLiterals = blockingLiterals.filter(_.arguments(i) == symbol)
              }
              symbol
            }

            if (blockingLiterals.isEmpty) {
              val decisionLiteral = Literal(AppRec(predicate, predicateArguments), polarity)
              decisions += decisionLiteral
              if (decisions.contains(!decisionLiteral)) {
                removeDecisionLiteral(!decisionLiteral)
              }
              reverseImplicationGraph(decisionLiteral) = ArrayBuffer(Decision(decisionLiteral))
              updateUnifiableUnits(Seq(decisionLiteral))
              chosen = true 
            }
          }
        } else {
          termDepthThreshold += 1
          updateUnifiableUnits(propagatedLiterals.filter(_.depth == termDepthThreshold).toSeq)
        }
      } else if (termDepthThreshold >= maxInitialTermDepth &&
                 cnf.clauses.forall(clause => clause.literals.exists(propagatedLiterals.contains))) {
        val literals      = propagatedLiterals ++ decisions
        val (positiveLiterals, negativeLiterals) = literals.partition(_.polarity)
        return Satisfiable(Some(new Assignment(positiveLiterals.map(_.unit).toSet ++ negativeLiterals.map(_.unit).toSet)))
      }
    }
    Error // this line is unreachable.
  }
  // scalastyle:on
}
