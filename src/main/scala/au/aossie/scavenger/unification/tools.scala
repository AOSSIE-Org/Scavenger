package au.aossie.scavenger.unification

import au.aossie.scavenger.expression._

/**
  * Created by vlad107 on 7/5/17.
  */
object tools {

  def isUnifyablePreChecking(left: E, right: E): Boolean = (left, right) match {
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
    case (App(f1: Sym, _), App(f2: Sym, _)) if f1 != f2 =>
      true
    case (AppRec(_: Sym, lArgs), AppRec(_: Sym, rArgs)) if lArgs.size != rArgs.size =>
      true
    case (AppRec(_: Sym, lArgs), AppRec(_: Sym, rArgs)) =>
      lArgs.zip(rArgs).exists { case (l, r) => isUnifyablePreChecking(l, r) }
    case _ =>
      false
  }
}
