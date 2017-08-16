package au.aossie.scavenger.prover.actors

import akka.actor.{Actor, ActorRef, ActorSystem}
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.proof.cr.{CRProof, CRProofNode, ConflictDrivenClauseLearning}
import au.aossie.scavenger.prover.{ProblemStatus, Unsatisfiable}
import au.aossie.scavenger.structure.immutable.{Clause, Literal}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.concurrent.Promise
import scala.util.Random

case class Start(initialClauses: Seq[Clause], friendActors: Seq[ActorRef])
case object ResolveUnitPropagation
case object ResolveCDCL
case class NewCDCLClauses(cdclNodes: Seq[CRProofNode])
case class RemoveCDCLPremises(premises: Set[Literal])
case class MakeDecisions(wasNewCDCLs: Boolean)


/**
  * Created by vlad107 on 7/27/17.
  */
class ExpertActor(predicates: Seq[Sym],
                  withSetOfSupport: Boolean,
                  promise: Promise[ProblemStatus],
                  maxIterationsWithoutDecision: Int)(implicit rnd: Random, implicit val system: ActorSystem) extends Actor {
  implicit val logger = Logger(LoggerFactory.getLogger("prover"))

  val expertData = new ExpertData(predicates.toSet, withSetOfSupport, maxIterationsWithoutDecision)
  val friendActors = new ListBuffer[ActorRef]

  override def receive: Receive = {
    case Start(initialClauses, actors) =>
      logger.debug("Start:")

      friendActors ++= actors
      predicates.foreach { case Sym(name) => logger.debug(name) }
      expertData.addInitialClauses(initialClauses)
      self ! ResolveUnitPropagation
    case ResolveUnitPropagation =>
      logger.debug("ResolveUnitPropagation")
      expertData.resolveUnitPropagation
      self ! ResolveCDCL
    case ResolveCDCL =>
      logger.debug("ResolveCDCL")
      val cdclNodes = expertData.resolveCDCL
      if (!promise.isCompleted) {
        cdclNodes.collect {
          case cdclNode@ConflictDrivenClauseLearning(conflict) if cdclNode.conclusion == Clause.empty =>
            promise.success(Unsatisfiable(Some(CRProof(conflict))))
        }
//        cdclNodes.foreach(node => println(node.conclusion))

        friendActors.foreach(_ ! NewCDCLClauses(cdclNodes))
        self ! RemoveCDCLPremises(cdclNodes.flatMap {
          case ConflictDrivenClauseLearning(conflict) =>
            conflict.decisions
        }(collection.breakOut))
      }
    case NewCDCLClauses(cdclNodes) =>
      logger.debug("NewCDCLClauses")

      expertData.addClauses(cdclNodes)
    case RemoveCDCLPremises(premises) =>
      logger.debug("RemoveCDCLPremises")

      expertData.removeCDCLPremises(premises)
      self ! MakeDecisions(premises.isEmpty)
    case MakeDecisions(wasNewCDCLs) =>
      logger.debug("MakeDecisions")

      expertData.makeDecision(wasNewCDCLs)
      self ! ResolveUnitPropagation

  }
}
