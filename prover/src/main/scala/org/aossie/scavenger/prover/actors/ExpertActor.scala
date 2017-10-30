package org.aossie.scavenger.prover.actors

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import org.aossie.scavenger.expression.Sym
import org.aossie.scavenger.proof.cr._
import org.aossie.scavenger.prover.{ProblemStatus, Unsatisfiable}
import org.aossie.scavenger.structure.immutable.{Clause, Literal}
import com.typesafe.config.Config
import com.typesafe.scalalogging.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.Promise
import scala.util.Random

case class Start(initialClauses: Seq[Clause], friendActors: Seq[ActorRef])
case object ResolveUnitPropagation
case object ResolveCdcl
case class NewCdclClauses(cdclNodes: Seq[CRProofNode])
case class RemoveCdclPremises(premises: Set[Literal])
case class MakeDecisions(wasNewCdcls: Boolean)

class ExpertActorPriorityMailbox(settings: ActorSystem.Settings, config: Config)
    extends UnboundedPriorityMailbox(
      PriorityGenerator {
        case _: Start               => 0
        case _: NewCdclClauses      => 1
        case ResolveUnitPropagation => 2
        case ResolveCdcl            => 3
        case RemoveCdclPremises     => 4
        case MakeDecisions          => 5
        case _                      => 1000
      }
    )

/**
 * Created by vlad107 on 7/27/17.
 */
class ExpertActor(
  predicates: Seq[Sym],
  withSetOfSupport: Boolean,
  promise: Promise[ProblemStatus],
  maxIterationsWithoutDecision: Int
)(implicit rnd: Random, implicit val system: ActorSystem)
    extends Actor {
  private val logger = Logger[this.type]

  val expertData   = new ExpertData(predicates.toSet, withSetOfSupport, maxIterationsWithoutDecision)
  val friendActors = new ListBuffer[ActorRef]

  override def receive: Receive = {
    case Start(initialClauses, actors) =>
      logger.debug("Start:")

      friendActors ++= actors
      logger.debug(predicates.map(_.name).mkString("\n"))
      expertData.addClauses(initialClauses.map(new InitialStatement(_)))
      self ! ResolveUnitPropagation
    case ResolveUnitPropagation =>
      logger.debug("ResolveUnitPropagation")

      expertData.resolveUnitPropagation()
      self ! ResolveCdcl
    case ResolveCdcl =>
      logger.debug("ResolveCdcl")

      val cdclNodes = expertData.resolveCdcl
      if (!promise.isCompleted) {
        cdclNodes.find(_.conclusion == Clause.empty) match {
          case Some(ConflictDrivenClauseLearning(conflict)) =>
            promise.success(Unsatisfiable(Some(CRProof(conflict))))
            context.stop(self)
          case None =>
            val (localNodes, globalNodes) = cdclNodes.partition { node =>
              Expertise(node, predicates.toSet).conclusion != Clause.empty
            }
            if (globalNodes.nonEmpty) {
              friendActors.foreach(_ ! NewCdclClauses(globalNodes))
            }
            if (localNodes.nonEmpty) {
              self ! NewCdclClauses(localNodes)
            }
            self ! RemoveCdclPremises(
              cdclNodes.flatMap {
                case ConflictDrivenClauseLearning(conflict) =>
                  conflict.decisions
              }.toSet
            )
        }
      }
    case NewCdclClauses(cdclNodes) =>
      logger.debug("NewCdclClauses")

      expertData.addClauses(cdclNodes)
    case RemoveCdclPremises(premises) =>
      logger.debug("RemoveCdclPremises")

      expertData.removeCdclPremises(premises)
      self ! MakeDecisions(premises.isEmpty)
    case MakeDecisions(wasNewCdcls) =>
      logger.debug("MakeDecisions")

      expertData.makeDecision(wasNewCdcls)
      self ! ResolveUnitPropagation
  }
}
