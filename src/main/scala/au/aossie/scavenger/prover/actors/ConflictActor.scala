package au.aossie.scavenger.prover.actors

import akka.actor.{ Actor, ActorLogging }
import akka.pattern.ask
import akka.util.Timeout
import au.aossie.scavenger.prover._
import au.aossie.scavenger.prover.actors.messages._
import au.aossie.scavenger.structure.immutable.{ Literal, SeqClause }
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr.{ Axiom, Decision, UnitPropagationResolution }
import au.aossie.scavenger.proof.cr.CRProofNode
import au.aossie.scavenger.proof.cr

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * @author Daniyar Itegulov
  */
class ConflictActor extends Actor with ActorLogging {
  private implicit val timeout: Timeout = 5 seconds

  // Proof for initial and cdcl clauses
  private val clauseProof = mutable.Map.empty[SeqClause, CRProofNode]

  val unifyingActor = context.actorSelection("../unify")
  val mainActor = context.actorSelection("../main")

  override def receive: Receive = {
    case Conflict(leftLiteral, rightLiteral, allClauses, decisions, reverseImpGraph) =>
      allClauses.foreach(clause => clauseProof(clause) = Axiom(clause))

      log.info(s"Conflict from $leftLiteral and $rightLiteral")

      /**
        * Finds conflict clause, used consisting of negation of all decisions-ancestors of `current`.
        *
        * @param current literal
        * @param substitution last instantiation of this literal
        * @return clause, representing disjunction of negated decision literals, used in propagation of current literal
        */
      def findConflictClause(current: Literal, substitution: Substitution = Substitution.empty): SeqClause = {
        if (allClauses contains current.toClause) {
          SeqClause.empty
        } else if (decisions contains current) {
          !substitution(current)
        } else if (reverseImpGraph contains current) {
          val conflictClauses = for ((clause, unifier) <- reverseImpGraph(current))
            yield unifier.map {
              case (lit, mgu) => findConflictClause(lit, mgu(substitution))
            }.fold(SeqClause.empty)(_ union _)
          conflictClauses.toSeq.sortBy(_.width).head
        } else {
          throw new IllegalStateException(s"Literal $current was propagated, but there is no history in implication graph")
        }
      }

      /**
        * Creates formal proof, which formally reasons `current` literal.
        *
        * @param current literal to be proved
        * @return formal proof, which conclusion is the `current`
        */
      def buildProof(current: Literal)(implicit variables: mutable.Set[Sym]): CRProofNode = {
        if (allClauses contains current.toClause) {
          Axiom(current.toClause)
        } else if (decisions contains current) {
          Decision(current)
        } else if (reverseImpGraph contains current) {
          val (clause, unifier) = reverseImpGraph(current).head
          val premiseProofs = unifier.map {
            case (lit, _) => buildProof(lit)
          }
          UnitPropagationResolution(premiseProofs, clauseProof(clause), current)
        } else {
          throw new IllegalStateException("Literal was propagated, but there is no history in implication graph")
        }
      }

      val variablesFuture = (unifyingActor ? GetVariables).mapTo[Set[Sym]]
      val future = (unifyingActor ? Unify(Seq(leftLiteral.unit), Seq(rightLiteral.unit)))
        .mapTo[Option[(Seq[Substitution], Substitution)]]

      Await.result(future.zip(variablesFuture), Duration.Inf) match {
        case (Some((Seq(leftMgu), rightMgu)), variables) =>
          implicit val vars = mutable.Set(variables.toSeq: _*)
          val conflictClauseLeft = findConflictClause(leftLiteral, leftMgu)
          val conflictClauseRight = findConflictClause(rightLiteral, rightMgu)
          val newClause = conflictClauseLeft union conflictClauseRight
          val conflictProof = cr.Conflict(buildProof(leftLiteral), buildProof(rightLiteral))
          clauseProof(newClause) = cr.ConflictDrivenClauseLearning(conflictProof)
          log.info(s"Derived conflict clause $newClause")
          mainActor ! Derived(newClause, reverseImpGraph, conflictProof)
        case _ =>
          log.error("leftLiteral and rightLiteral should be unifiable")
      }
    case other =>
      log.warning(s"Didn't expect, but got $other")
  }
}

