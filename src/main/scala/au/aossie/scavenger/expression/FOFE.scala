package au.aossie.scavenger.expression

import au.aossie.scavenger.expression.{formula => Form}
import au.aossie.scavenger.structure.immutable.{CNF, Clause}

import scala.collection.immutable.ListSet


/**
  * @author Vlad Podtelkin
  */

sealed trait FOFE {
  def freeVars(): Set[FVar]
  override def equals(obj: Any): Boolean = obj match {
    case other: FOFE => other.toString == this.toString
    case _           => false
  }
  override def hashCode(): Int = this.toString.hashCode()
}

sealed trait FTerm extends FOFE {
  def skolemizeRec(toFun: Map[FVar, FFun]): FTerm
  def renameVars(toOrig: Map[FVar, FVar]): FTerm
  def toE(quantified: Set[FVar]): E
}

sealed trait FProp extends FOFE {
  def miniScope(): FProp = this
  def negationsIn(): FProp
  def skolemize(): FProp = {
    skolemizeRec(Set[FVar](), Map[FVar, FFun](), 0)._1
  }
  def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], ind: Int): (FProp, Int)
  def forallOut(): FProp = {
    val (prop, count) = forallOutRec(Map[FVar, FVar](), 0)
    var res = prop
    for (i <- 0 until count) res = FForAll(FVar("X" + i), res)
    res
  }
  def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int)
  def toCNF: CNF = CNF(toCNFRec(Set[FVar](), 0)._1.toList)
  def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int)
}

sealed trait FQuantifier extends FProp

case class FVar(name: String) extends FTerm {
  override def toString: String = name
  override def freeVars(): Set[FVar] = Set(this)

  override def skolemizeRec(toFun: Map[FVar, FFun]): FTerm =
    toFun.get(this) match {
      case Some(fun) => fun
      case None      => this
    }

  override def renameVars(toOrig: Map[FVar, FVar]): FTerm =
    toOrig.get(this) match {
      case Some(v) => v
      case None    => this
    }

  override def toE(quantified: Set[FVar]): E =
    if (quantified contains this) {
      Var(name)
    } else {
      Sym(name)
    }
}

case class FFun(name: String, args: List[FTerm]) extends FTerm {
  override def toString: String = name + "(" + args.mkString(", ") + ")"
  override def freeVars(): Set[FVar] = args.map(_.freeVars()).foldLeft(Set[FVar]())(_ union _)

  override def skolemizeRec(toFun: Map[FVar, FFun]): FTerm =
    FFun(name, args.map(_.skolemizeRec(toFun)))

  override def renameVars(toOrig: Map[FVar, FVar]): FTerm =
    FFun(name, args.map(_.renameVars(toOrig)))

  override def toE(quantified: Set[FVar]): E = AppRec(Sym(name), args.map(_.toE(quantified)))
}

case class FConjunction(lhs: FProp, rhs: FProp) extends FProp {
  override def toString: String = "(" + lhs + " & " + rhs + ")"
  override def freeVars(): Set[FVar] = lhs.freeVars() ++ rhs.freeVars()

  override def negationsIn(): FProp = FConjunction(lhs.negationsIn(), rhs.negationsIn())

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) = {
    val (nlhs, indexl) = lhs.skolemizeRec(curVars, toFun, index)
    val (nrhs, indexr) = rhs.skolemizeRec(curVars, toFun, indexl)
    (FConjunction(nlhs, nrhs), indexr)
  }

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) = {
    val (nlhs, indexl) = lhs.forallOutRec(toOrig, index)
    val (nrhs, indexr) = rhs.forallOutRec(toOrig, indexl)
    (FConjunction(nlhs, nrhs), indexr)
  }

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) = {
    val (nlhs, indexl) = lhs.toCNFRec(quantified, index)
    val (nrhs, indexr) = rhs.toCNFRec(quantified, indexl)
    (nlhs ++ nrhs, indexr)
  }
}

case class FDisjunction(lhs: FProp, rhs: FProp) extends FProp {
  override def toString: String = "(" + lhs + " | " + rhs + ")"
  override def freeVars(): Set[FVar] = lhs.freeVars() ++ rhs.freeVars()

  override def negationsIn(): FProp = FDisjunction(lhs.negationsIn(), rhs.negationsIn())

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) = {
    val (nlhs, indexl) = lhs.skolemizeRec(curVars, toFun, index)
    val (nrhs, indexr) = rhs.skolemizeRec(curVars, toFun, indexl)
    (FDisjunction(nlhs, nrhs), indexr)
  }

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) = {
    val (nlhs, indexl) = lhs.forallOutRec(toOrig, index)
    val (nrhs, indexr) = rhs.forallOutRec(toOrig, indexl)
    (FDisjunction(nlhs, nrhs), indexr)
  }

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) = {
    val z = AppRec(Sym("predicate" + index), List[E]())
    val (nlhs, indexl) = lhs.toCNFRec(quantified, index + 1)
    val (nrhs, indexr) = rhs.toCNFRec(quantified, indexl)
    (nlhs.map(_ + z) ++ nrhs.map(z +: _), indexr)
  }
}

case class FNegation(prop: FProp) extends FProp {
  override def toString: String = "~" + prop
  override def freeVars(): Set[FVar] = prop.freeVars()

  override def negationsIn(): FProp = prop match {
    case FConjunction(lhs, rhs) => FDisjunction(FNegation(lhs).negationsIn(), FNegation(rhs).negationsIn())
    case FDisjunction(lhs, rhs) => FConjunction(FNegation(lhs).negationsIn(), FNegation(rhs).negationsIn())
    case FNegation(p)           => p.negationsIn()
    case FForAll(x, p)          => FExists(x, FNegation(p).negationsIn())
    case FExists(x, p)          => FForAll(x, FNegation(p).negationsIn())
    case _                      => this
  }

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) = {
    val (nprop, nindex) = prop.skolemizeRec(curVars, toFun, index)
    (FNegation(nprop), nindex)
  }

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) = {
    val (nprop, nindex) = prop.forallOutRec(toOrig, index)
    (FNegation(nprop), nindex)
  }

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) = {
    val (nprop, nindex) = prop.toCNFRec(quantified, index)
    (nprop.map((c: Clause) => Clause(c.suc.toList: _*)(c.ant.toList: _*)), index)
  }
}

case class FPredicate(name: String, args: List[FTerm]) extends FProp {
  override def toString: String = name + "(" + args.mkString(", ") + ")"
  override def freeVars(): Set[FVar] = args.map(_.freeVars()).foldLeft(Set[FVar]())(_ union _)
  override def negationsIn(): FProp = this

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) =
    (FPredicate(name, args.map(_.skolemizeRec(toFun))), index)

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) =
    (FPredicate(name, args.map(_.renameVars(toOrig))), index)

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) = {
    (ListSet[Clause](Clause()(AppRec(Sym(name), args.map(_.toE(quantified))))), index)
  }
}

case class FForAll(x: FVar, prop: FProp) extends FQuantifier {
  override def toString: String = "(" + "!" + x + "." + prop + ")"
  override def freeVars(): Set[FVar] = prop.freeVars() - x
  override def negationsIn(): FProp = FForAll(x, prop.negationsIn())

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) = {
    val (nprop, nindex) = prop.skolemizeRec(curVars + x, toFun, index)
    (FForAll(x, nprop), nindex)
  }

  override def miniScope(): FProp = {
    if (prop.freeVars contains x) {
      prop match {
        case FConjunction(lhs: FProp, rhs: FProp) =>
          FConjunction(FForAll(x, lhs).miniScope(), FForAll(x, rhs).miniScope())
        case _ => FForAll(x, prop.miniScope())
      }
    } else prop.miniScope()
  }

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) =
    prop.forallOutRec(toOrig + (x -> FVar("X" + index)), index + 1)

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) =
    prop.toCNFRec(quantified + x, index)
}

case class FExists(x: FVar, prop: FProp) extends FQuantifier {
  override def toString: String = "(" + "?" + x + "." + prop + ")"
  override def freeVars(): Set[FVar] = prop.freeVars() - x
  override def negationsIn(): FProp = FExists(x, prop.negationsIn())

  override def skolemizeRec(curVars: Set[FVar], toFun: Map[FVar, FFun], index: Int): (FProp, Int) = {
    val fun = FFun("skolemization" + index, curVars.toList)
    prop.skolemizeRec(curVars, toFun + (x -> fun), index + 1)
  }

  override def miniScope(): FProp = {
    if (prop.freeVars contains x) {
      prop match {
        case FDisjunction(lhs: FProp, rhs: FProp) => FDisjunction(FExists(x, lhs).miniScope(), FExists(x, rhs).miniScope())
        case _ => FExists(x, prop.miniScope())
      }
    } else prop.miniScope()
  }

  override def forallOutRec(toOrig: Map[FVar, FVar], index: Int): (FProp, Int) =
    throw new RuntimeException("exists should be available on this stage")

  override def toCNFRec(quantified: Set[FVar], index: Int): (ListSet[Clause], Int) =
    throw new RuntimeException("exists should be available on this stage")
}

object FOFE {
  /**
    * Translates formula from type E to my data type FOFE.
    * Removes implications (A ->  B == !A or B)
    * Removes equivalences (A <=> B == (!A or B) and (!B or A))
   */
  def toProp(formula: E): FProp = {
    formula match {
      case App(App(Sym(Form.impS), lhs), rhs)        => implToDisj(toProp(lhs), toProp(rhs))
      case App(App(Sym(Form.equivS), lhs), rhs)      => {
        val lhs_prop = toProp(lhs)
        val rhs_prop = toProp(rhs)
        FConjunction(implToDisj(lhs_prop, rhs_prop),
                     implToDisj(rhs_prop, lhs_prop))
      }
      case App(App(Sym(Form.andS), lhs), rhs)        => FConjunction(toProp(lhs), toProp(rhs))
      case App(App(Sym(Form.orS), lhs), rhs)         => FDisjunction(toProp(lhs), toProp(rhs))
      case App(Sym(Form.negS), prop)                 => FNegation(toProp(prop))
      case App(Sym(Form.allS), Abs(Var(x), _, prop)) => FForAll(FVar(x), toProp(prop))
      case App(Sym(Form.exS), Abs(Var(x), _, prop))  => FExists(FVar(x), toProp(prop))
      case AppRec(Sym(name), args)                   => FPredicate(name, args.toList map toTerm)
    }
  }
  def toTerm(formula: E): FTerm = {
    formula match {
      case Var(name)               => FVar(name)
      case AppRec(Sym(name), args) => FFun(name, args.toList map toTerm)
    }
  }

  def implToDisj(lhs: FProp, rhs: FProp): FProp = FDisjunction(FNegation(lhs), rhs)
}
