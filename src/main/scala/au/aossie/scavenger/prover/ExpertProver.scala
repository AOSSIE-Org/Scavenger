package au.aossie.scavenger.prover
import akka.actor.{ActorSystem, Props}
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.prover.actors.{ExpertActor, Start}
import au.aossie.scavenger.structure.immutable.CNF

import scala.collection.mutable
import scala.util.Random

/**
  * Created by vlad107 on 7/27/17.
  */
class ExpertProver(numActors: Int, withSetOfSupport: Boolean) {

  implicit val rnd = new Random(107)

  def prove(cnf: CNF): Unit = {
    val occurenciesCount = cnf.clauses
      .flatMap(_.predicates.map(_._1))
      .groupBy(identity)
      .mapValues(_.size).toSeq
      .sortBy(_._2).reverse

    val predicatesPerActor = mutable.ListBuffer.fill(numActors)((mutable.ListBuffer.empty[Sym], 0))
    for ((sym, cnt) <- occurenciesCount) {
      val ((acc, curCnt), index) =
        predicatesPerActor.zipWithIndex.minBy(_._1._2)
      predicatesPerActor.update(index, (acc += sym, curCnt + cnt))
    }

    implicit val system = ActorSystem("expertSystem")
    val experts = Seq.tabulate(numActors) {
      id =>
        system.actorOf(Props(new ExpertActor(predicatesPerActor(id)._1, withSetOfSupport)), name = s"expert$id")
    }
    experts.foreach(_ ! Start(cnf.clauses))

    system.terminate()
  }
}

object ExpertProver extends ExpertProver(numActors = 4, withSetOfSupport = true)