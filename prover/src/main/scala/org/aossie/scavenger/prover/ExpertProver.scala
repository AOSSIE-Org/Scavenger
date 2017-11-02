package org.aossie.scavenger.prover

import akka.actor.{ActorSystem, Props}
import org.aossie.scavenger.expression.Sym
import org.aossie.scavenger.proof.cr.{CRProof, InitialStatement}
import org.aossie.scavenger.prover.actors.{ExpertActor, Start}
import org.aossie.scavenger.structure.immutable.{CNF, Clause}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Random

/**
 * Created by vlad107 on 7/27/17.
 */
class ExpertProver(
  numActors: Int,
  withSetOfSupport: Boolean,
  maxIterationsWithoutDecision: Int
) extends Prover {

  implicit private val rnd: Random = new Random(107)

  implicit private val system: ActorSystem = ActorSystem("default", ConfigFactory.load)

  def prove(cnf: CNF, duration: Duration = Duration.Inf): ProblemStatus = {
    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(CRProof(InitialStatement(Clause.empty))))
    }

    val occurrencesCount = cnf.clauses
      .flatMap(_.predicates.map(_._1))
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
      .sortBy(_._2)
      .reverse

    val predicatesPerActor = mutable.ListBuffer.fill(numActors)((mutable.ListBuffer.empty[Sym], 0))
    for ((sym, cnt) <- occurrencesCount) {
      val ((acc, curCnt), index) =
        predicatesPerActor.zipWithIndex.minBy(_._1._2)
      predicatesPerActor.update(index, (acc += sym, curCnt + cnt))
    }

    val promise = Promise[ProblemStatus]()
    val experts = Seq.tabulate(numActors) { id =>
      system.actorOf(
        Props(
          new ExpertActor(
            predicatesPerActor(id)._1,
            withSetOfSupport,
            promise,
            maxIterationsWithoutDecision
          )
        ).withDispatcher("prio-dispatcher")
      )
    }
    experts.foreach { expertActor =>
      expertActor ! Start(cnf.clauses, experts)
    }
    Await.result(promise.future, Duration.Inf)
  }
}

object ExpertProver
    extends ExpertProver(
      numActors = 4,
      withSetOfSupport = true,
      maxIterationsWithoutDecision = 10
    )
