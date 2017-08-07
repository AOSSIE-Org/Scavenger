package au.aossie.scavenger.prover.actors

import akka.actor.{Actor, ActorSystem}
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.Clause

import scala.util.Random

case class Start(initialClauses: Seq[Clause])
case object ResolveUnitPropagation
case object ResolveCDCL

/**
  * Created by vlad107 on 7/27/17.
  */
class ExpertActor(predicates: Seq[Sym], withSetOfSupport: Boolean)(implicit rnd: Random, implicit val system: ActorSystem) extends Actor {
  val expertData = new ExpertData(predicates.toSet, withSetOfSupport)

  override def receive: Receive = {
    case Start(initialClauses) =>
      expertData.addInitialClauses(initialClauses)
      self ! ResolveUnitPropagation
    case ResolveUnitPropagation =>
      expertData.resolveUnitPropagation
      self ! ResolveCDCL
    case ResolveCDCL =>
      expertData.resolveCDCL


  }


}
