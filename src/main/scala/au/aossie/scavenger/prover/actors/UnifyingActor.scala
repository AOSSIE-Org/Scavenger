package au.aossie.scavenger.prover.actors

import akka.actor.{Actor, ActorLogging}
import au.aossie.scavenger.prover.actors.messages.{GetVariables, Unify}
import au.aossie.scavenger.prover._
import au.aossie.scavenger.expression.Var

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
class UnifyingActor(implicit variables: mutable.Set[Var]) extends Actor with ActorLogging {

  override def receive: Receive = {
    case Unify(left, right) =>
      sender ! unifyWithRename(left.map(_.unit), right.map(_.unit))
    case GetVariables =>
      sender ! variables.toSet
    case other =>
      log.warning(s"Didn't expect, but got $other")
  }
}
