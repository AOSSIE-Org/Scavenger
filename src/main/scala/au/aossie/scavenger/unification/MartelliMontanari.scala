package au.aossie.scavenger.unification

import collection.mutable.{ HashMap => MMap, Set => MSet }
import au.aossie.scavenger.expression.{ E, Sym, App, Abs }
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.expression.substitution.mutable.{ Substitution => MSub }

object MartelliMontanari {

  def apply(a: E, b: E)(implicit variables: MSet[Sym]): Option[Substitution] = apply(List((a,b)))
  
  def apply(equations: Iterable[(E, E)])(implicit variables: MSet[Sym]): Option[Substitution] = {
    var eqs = equations.toSeq
    val mgu = new MSub

    var counter = 0

    while (!eqs.isEmpty) {

//            println("mgu: " + counter)

//      counter = counter + 1
//            if (counter > 50) { //10 is too small.
//              println("counter maxed out")
//              return None
//              println("mgu: " + mgu)
//            }

      eqs.head match {
        case (App(f1, a1), App(f2, a2)) => eqs = Seq((f1, f2), (a1, a2)) ++ eqs.tail
        case (Abs(v1, e1), Abs(v2, e2)) => eqs = Seq((v1, v2), (e1, e2)) ++ eqs.tail
        case (v1: Sym, v2: Sym) if (v1 == v2) => eqs = eqs.tail
        case (v: Sym, e: E) if variables contains v => {
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
        case (e: E, v: Sym) if variables contains v => eqs = Seq((v, e)) ++ eqs.tail
        case _ => return {
          None
        }
      }
    }
    return Some(mgu.toImmutable)
  }
}
 
