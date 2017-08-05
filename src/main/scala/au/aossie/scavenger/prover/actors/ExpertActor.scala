package au.aossie.scavenger.prover.actors

import akka.actor.Actor
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.structure.immutable.Clause

import scala.util.Random

case class Start(initialClauses: Seq[Clause])
case object ResolveUnitPropagation

/**
  * Created by vlad107 on 7/27/17.
  */
class ExpertActor(predicates: Seq[Sym])(implicit rnd: Random) extends Actor {
  val expertData = new ExpertData(predicates.toSet)

  override def receive: Receive = {
    case Start(initialClauses) =>
      expertData.addNewClauses(initialClauses)
      self ! ResolveUnitPropagation
    case ResolveUnitPropagation =>
      expertData.resolveUnitPropagation

  }


}
