package au.aossie.scavenger.prover

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import au.aossie.scavenger.prover.actors.{ConflictActor, MainActor, PropagationActor, UnifyingActor}
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.proof.cr.{CRProof => Proof}

import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}

import scala.language.postfixOps

/**
  * @author Daniyar Itegulov
  */
object ConcurrentCR extends Prover {
  def prove(cnf: CNF)(implicit variables: mutable.Set[Sym]): Option[Proof] = {
    implicit val timeout: Timeout = 2 seconds
    implicit val system = ActorSystem()
    val unifyingActor = system.actorOf(Props(new UnifyingActor()), "unify")
    val conflictActor = system.actorOf(Props(new ConflictActor()), "conflict")
    val propagationActor = system.actorOf(Props(new PropagationActor(unifyingActor)), "propagate")
    val mainActor = system.actorOf(Props(new MainActor(cnf, propagationActor, conflictActor)), "main")

    val future = (mainActor ? "promise").mapTo[Promise[Option[Proof]]]
    val duration = Duration.Inf
    val promise = Await.result(future, duration)
    val result = Await.result(promise.future, duration)
    Await.ready(system.terminate(), Duration.Inf)
    result
  }
}
