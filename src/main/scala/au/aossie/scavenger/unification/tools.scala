package au.aossie.scavenger.unification

import au.aossie.scavenger.expression._

/**
  * Created by vlad107 on 7/5/17.
  */
object tools {

  def disunifiableQuickCheck(left: E, right: E): Boolean = (left, right) match {
    case (Var(_), _) =>
      false
    case (_, Var(_)) =>
      false
    case (Sym(c1), Sym(c2)) =>
      c1 != c2
    case (App(_, _), Sym(_)) =>
      true
    case (Sym(_), App(_, _)) =>
      true
    case (AppRec(Sym(fun1), lArgs), AppRec(Sym(fun2), rArgs)) =>
      if ((fun1 != fun2) || (lArgs.size != rArgs.size))
        true
      else
        lArgs.zip(rArgs).exists { case (l, r) => disunifiableQuickCheck(l, r) }
    case _ =>
      false
  }
}
