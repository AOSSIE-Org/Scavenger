package au.aossie.scavenger.parser

import au.aossie.scavenger.expression.formula._
import au.aossie.scavenger.expression._
import au.aossie.scavenger.expression.term.{FunctionTerm}
import au.aossie.scavenger.structure.immutable.{CNF, Clause}

import scala.collection.immutable.ListSet

/**
  * Created by vlad107 on 3/3/17.
  */
object TPTPClausifier {
  def clausify(expr: E): CNF = {
    val cnfForm = toCNF(skolem(negIn(
      miniscope(
        remImpl(expr)))))
    println(cnfForm)
    cnfForm
  }
  def remImpl(f: E): E = f match {
    case Neg(g)              => Neg(remImpl(g))
    case And(a, b)           => And(remImpl(a), remImpl(b))
    case Or(a,b)             => Or(remImpl(a), remImpl(b))
    case All(v, t, g)        => All(v, t, remImpl(g))
    case Ex(v, t, g)         => Ex(v, t, remImpl(g))
    case Atom(g, args)       => f
    case Imp((a, b))         => Or(Neg(remImpl(a)), remImpl(b))
    case Equivalence((a, b)) =>
      val na = remImpl(a)
      val nb = remImpl(b)
      And(Or(Neg(na), nb), Or(Neg(nb), na))
  }
  def miniscope(f: E): E = f match {
    case All(v, t, g) if !(g.freeVariables contains v) => miniscope(g)
    case All(v, t, And(a, b))                          =>
      And(miniscope(All(v, t, a)), miniscope(All(v, t, b)))
    case All(v, t, g)          => All(v, t, miniscope(g))
    case Ex(v, t, g) if !(g.freeVariables contains v)  => miniscope(g)
    case Ex(v, t, Or(a, b))                            => Or(miniscope(Ex(v, t, a)), miniscope(Ex(v, t, b)))
    case Ex(v, t, g)                                   => Ex(v, t, miniscope(g))
    case Neg(g)                                        => Neg(miniscope(g))
    case And(a, b)                                     => And(miniscope(a), miniscope(b))
    case Or(a, b)                                      => Or(miniscope(a), miniscope(b))
    case _                                             => f
  }
  def negIn(f: E): E = f match {
    case Neg(And(a, b))    => Or(negIn(Neg(a)), negIn(Neg(b)))
    case Neg(Or(a, b))     => And(negIn(Neg(a)), negIn(Neg(b)))
    case Neg(Ex(v, t, g))  => All(v, t, negIn(g))
    case Neg(All(v, t, g)) => Ex(v, t, negIn(g))
    case Neg(Neg(g))       => negIn(g)
    case And(a, b)         => And(negIn(a), negIn(b))
    case Or(a, b)          => Or(negIn(a), negIn(b))
    case All(v, t, g)      => All(v, t, negIn(g))
    case Ex(v, t, g)       => Ex(v, t, negIn(g))
    case _                 => f
  }
  def skolem(f: E): E = {
    def skolemRec(h: E, curVars: Set[Var], toFun: Map[Var, E], index: Int): (E, Int) = h match {
      case Or(a, b) =>
        val (na, index1) = skolemRec(a, curVars, toFun, index)
        val (nb, index2) = skolemRec(b, curVars, toFun, index1)
        (Or(na, nb), index2)
      case And(a, b) =>
        val (na, index1) = skolemRec(a, curVars, toFun, index)
        val (nb, index2) = skolemRec(b, curVars, toFun, index1)
        (And(na, nb), index2)
      case Neg(a) =>
        val (na, index1) = skolemRec(a, curVars, toFun, index)
        (Neg(na), index1)
      case All(v, t, a) =>
        val (na, index1) = skolemRec(a, curVars + v, toFun, index)
        (All(v, t, na), index1)
      case Ex(v, t, a) =>
        // TODO: can be collision with original functions from input
        val fun = FunctionTerm("skolemize" + index, curVars.toList)
        skolemRec(a, curVars, toFun + (v -> fun), index + 1)
      case Atom(g, args) =>
        (Atom(g, args.toList.map(substitute(_, toFun))), index)
    }
    skolemRec(f, Set[Var](), Map[Var, E](), 0)._1
  }
  def forallOut(f: E): E = {
    def forallOutRec(h: E, toOrig: Map[Var, Var], index: Int): (E, Int) = h match {
      case Or(a, b) =>
        val (na, index1) = forallOutRec(a, toOrig, index)
        val (nb, index2) = forallOutRec(b, toOrig, index1)
        (Or(na, nb), index2)
      case And(a, b) =>
        val (na, index1) = forallOutRec(a, toOrig, index)
        val (nb, index2) = forallOutRec(b, toOrig, index1)
        (And(na, nb), index2)
      case Neg(a) =>
        val (na, index1) = forallOutRec(a, toOrig, index)
        (Neg(na), index1)
      case All(v, t, a) =>
        forallOutRec(a, toOrig + (v -> Var("MYOWNX" + index)), index + 1)
      case Atom(g, args) =>
        (substitute(AppRec(g, args), toOrig), index)
    }
    forallOutRec(f, Map[Var, Var](), 0)._1
  }
  def toCNF(f: E): CNF = {
    def toCNFRec(h: E, index: Int): (ListSet[Clause], Int) = h match {
      case And(a, b) =>
        val (l1, index1) = toCNFRec(a, index)
        val (l2, index2) = toCNFRec(b, index1)
        (l1 ++ l2, index2)
      case Or(a, b) =>
        val z = AppRec(Sym("predicate" + index), List[E]())
        val (l1, index1) = toCNFRec(a, index + 1)
        val (l2, index2) = toCNFRec(b, index1)
        (l1.map(_ + z) ++ l2.map(z +: _), index2)
      case Neg(a) =>
        val (l, index1) = toCNFRec(a, index)
        (l.map((c: Clause) => Clause(c.suc.toList: _*)(c.ant.toList: _*)), index1)
      case All(v, t, a) =>
        toCNFRec(a, index)
      case Atom(g, args) =>
        (ListSet[Clause](Clause()(AppRec(g, args))), index)
    }
    CNF(toCNFRec(f, 0)._1.toList)
  }
  def substitute(term: E, toFun: Map[Var, E]): E = term match {
    case FunctionTerm(f, args) => AppRec(f, args.map(substitute(_, toFun)))
    case Sym(name)             => Sym(name)
    case Var(name)             => toFun.get(Var(name)) match {
      case Some(fun) => fun
      case None      => Var(name)
    }
  }
}

