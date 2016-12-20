package au.aossie.scavenger.unification

import collection.mutable.{ HashMap => MMap, Set => MSet }
import au.aossie.scavenger.expression.{ E, Sym, Var, App, Abs }
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.substitution.mutable.{ Substitution => MSub }

object MartelliMontanari {

  def apply(a: E, b: E): Option[Substitution] = apply(List((a,b)))

  // scalastyle:off cyclomatic.complexity
  def apply(equations: Iterable[(E, E)]): Option[Substitution] = {
    var eqs = equations.toSeq
    val mgu = new MSub

    var counter = 0

    // scalastyle:off return
    while (!eqs.isEmpty) {
      eqs.head match {
        case (App(f1, a1), App(f2, a2)) => eqs = Seq((f1, f2), (a1, a2)) ++ eqs.tail
        case (Abs(v1, t1, e1), Abs(v2, t2, e2)) => if (t1 == t2) eqs = Seq((v1, v2), (e1, e2)) ++ eqs.tail else return None
        case (v1: Sym, v2: Sym) if (v1 == v2) => eqs = eqs.tail
        case (v: Var, e: E) => {
          if (v.occursIn(e)) return None
          // without occur-check
          val sub = Substitution(v -> e)
          for (p <- mgu) {
            mgu.update(p._1, sub(p._2))
          }
          mgu += (v -> e)
          eqs = for (eq <- eqs.tail) yield {
            (sub(eq._1), sub(eq._2))
          }

        }
        case (e: E, v: Var) => eqs = Seq((v, e)) ++ eqs.tail // TODO: Performance could be improved here.
        case _ => return None
      }
    }
    return Some(mgu.toImmutable)
    // scalastyle:on return
  }
  // scalastyle:on cyclomatic.complexity
}

