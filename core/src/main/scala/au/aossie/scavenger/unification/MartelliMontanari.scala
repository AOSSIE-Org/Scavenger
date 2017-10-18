package au.aossie.scavenger.unification

import au.aossie.scavenger.expression.{Abs, App, E, Sym, Var}
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.substitution.mutable.{Substitution => MSub}

import scala.annotation.tailrec

object MartelliMontanari {

  def apply(a: E, b: E): Option[Substitution] = apply(List((a, b)))

  def apply(equations: Iterable[(E, E)]): Option[Substitution] = {
    val mgu = new MSub

    @tailrec
    def applyRec(eqs: List[(E, E)]): Option[Substitution] = {
      eqs match {
        case (App(f1, a1), App(f2, a2)) :: eqsTails =>
          applyRec((f1, f2) :: (a1, a2) :: eqsTails)
        case (Abs(v1, t1, e1), Abs(v2, t2, e2)) :: eqsTails if t1 == t2 =>
          applyRec((v1, v2) :: (e1, e2) :: eqsTails)
        case (Abs(_, _, _), Abs(_, _, _)) :: _ =>
          None
        case (v1: Sym, v2: Sym) :: eqsTails if v1 == v2 =>
          applyRec(eqsTails)
        case (v: Var, e: E) :: _ if v.occursIn(e) =>
          None
        case (v: Var, e: E) :: eqsTails =>
          val sub = Substitution(v -> e)
          for ((k, v) <- mgu) {
            mgu.update(k, sub(v))
          }
          mgu += v -> e
          applyRec(eqsTails.map { case (leq, req) => (sub(leq), sub(req)) })
        case (e: E, v: Var) :: eqsTails =>
          applyRec((v, e) +: eqsTails)
        case _ :: _ =>
          None
        case Nil =>
          Some(mgu.toImmutable)
      }
    }
    applyRec(equations.toList)
  }
}
