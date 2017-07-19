package au.aossie.scavenger.prover.util

import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.proof.cr._
import au.aossie.scavenger.structure.immutable.{Clause, Literal}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by vlad107 on 7/14/17.
  */
class DecisionMaker(initialBump: Double,
                    decayFactor: Double,
                    maxActivity: Double,
                    randomDecisionsPercent: Double)(implicit val rnd: Random) {

  val activity: mutable.Map[Sym, Double] = mutable.HashMap.empty
  var incSym: Double = initialBump
  var temp: Double = 20

  def reset(): Unit = {
    activity.clear()
    incSym = initialBump
    temp = 20
  }

  def bumpActivity(literal: Literal): Unit = {
    val syms = literal.toClause.predicates.map(_._1)
    var overhead: Boolean = false
    syms.foreach { sym =>
      activity.update(sym, activity.getOrElseUpdate(sym, 0) + incSym)
      overhead |= (activity(sym) >= maxActivity)
    }
    if (overhead) {
      activity.transform {
        case (key, value) => value / maxActivity
      }
      incSym /= maxActivity
    }
  }

  def bumpActivityMiniSAT(node: CRProofNode): Unit = node match {
    case Decision(literal) =>
      bumpActivity(literal)
    case InitialStatement(clause) =>
      clause.literals.foreach(bumpActivity)
    case ConflictDrivenClauseLearning(conflict) =>
      bumpActivityMiniSAT(conflict)
    case Conflict(left, right) =>
      bumpActivityMiniSAT(left)
      bumpActivityMiniSAT(right)
    case UnitPropagationResolution(left, right, _, _, _) =>
      left.foreach(bumpActivityMiniSAT)
      bumpActivityMiniSAT(right)
  }

  def getActivity(literal: Literal): Double = {
    val funs = literal.toClause.predicates
    funs.map {
      case (sym, arity) => activity.getOrElse(sym, 0.0)
    }.sum * 1.0 / funs.size
  }

  /**
    * Implementation of miniSAT version of VSIDS heuristic
    *
    * @param available set of candidates for choosing as a new decision
    * @return new decision
    */
  def makeDecision(available: Seq[Literal]): Literal = {
    val res = {
      if (rnd.nextInt(101) >= randomDecisionsPercent) {
        val availableActivities = available.map(getActivity)
        val availableActivitiesSum = availableActivities.sum
        val probs = availableActivities.map(act => math.exp(temp * (act / availableActivitiesSum)))
        val p = rnd.nextDouble * probs.sum
        var curP = 0.0
        for ((literal, prob) <- available.zip(probs)) {
          curP += prob
          if (p <= curP) {
            return literal
          }
        }
      }
      available(rnd.nextInt(available.size))
    }
    res
  }

  def update(newLiterals: Seq[Clause]): Unit = {
    newLiterals.flatMap(_.literals)(collection.breakOut).foreach(bumpActivity)
    incSym /= decayFactor
    temp = 1.0 max (temp * 0.99)
  }
}
