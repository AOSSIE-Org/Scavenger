package org.aossie.scavenger.preprocessing

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import org.aossie.scavenger.parser.FunctionTerm
import org.aossie.scavenger.structure.immutable.{CNF, Clause, ClauseType}

import scala.collection.mutable.ListBuffer

/**
  * Created by vlad107 on 3/3/17.
  * Implementation of TPTP algorithm from [[http://www.cs.miami.edu/home/geoff/Papers/Journal/1996_SM96_SACJ.pdf here]]
  */
class TPTPClausifier {
  def apply(exprs: List[(E, ClauseType)]): CNF = {
    indexSkolem = 0
    indexVariable = 0
    indexPredicate = 0

    exprs.map { case (expr, clauseType) =>
      val uniqueVarsExpr = makeVarsUnique(expr)
      toCNF(
        removeForAll(
          skolem(
            negIn(
              miniscope(
                remImpl(uniqueVarsExpr))))), clauseType)
    } reduce (_ + _)
  }

  var indexSkolem: Int = 0
  var indexVariable: Int = 0
  var indexPredicate: Int = 0

  def makeVarsUnique(e: E): E = {
    def rec(e: E, sub: Map[Var, Var]): E = e match {
      case Neg(g) => Neg(rec(g, sub))
      case And(a, b) => And(rec(a, sub), rec(b, sub))
      case Or(a, b) => Or(rec(a, sub), rec(b, sub))
      case All(v, t, g) =>
        val newVar = Var(s"X_$indexVariable")
        indexVariable += 1
        All(newVar, t, rec(g, sub + (v -> newVar)))
      case Ex(v, t, g) =>
        val newVar = Var(s"X_$indexVariable")
        indexVariable += 1
        Ex(newVar, t, rec(g, sub + (v -> newVar)))
      case Imp(a, b) => Or(Neg(rec(a, sub)), rec(b, sub))
      case Equivalence(a, b) => Equivalence(rec(a, sub), rec(b, sub))
      case term@AppRec(g, args) => substitute(term, sub)
    }
    rec(e, Map.empty)
  }

  def remImpl(f: E): E = f match {
    case Neg(g) => Neg(remImpl(g))
    case And(a, b) => And(remImpl(a), remImpl(b))
    case Or(a, b) => Or(remImpl(a), remImpl(b))
    case All(v, t, g) => All(v, t, remImpl(g))
    case Ex(v, t, g) => Ex(v, t, remImpl(g))
    case Imp(a, b) => Or(Neg(remImpl(a)), remImpl(b))
    case Equivalence(a, b) =>
      val na = remImpl(a)
      val nb = remImpl(b)
      And(Or(Neg(na), nb), Or(Neg(nb), na))
    case Atom(g, args) => f
  }

  def miniscope(f: E): E = f match {
    case All(v, t, g) if !(g.freeVariables contains v) => miniscope(g)
    case All(v, t, And(a, b)) => And(miniscope(All(v, t, a)), miniscope(All(v, t, b)))
    case All(v, t, g) => All(v, t, miniscope(g))
    case Ex(v, t, g) if !(g.freeVariables contains v) => miniscope(g)
    case Ex(v, t, Or(a, b)) => Or(miniscope(Ex(v, t, a)), miniscope(Ex(v, t, b)))
    case Ex(v, t, g) => Ex(v, t, miniscope(g))
    case Neg(g) => Neg(miniscope(g))
    case And(a, b) => And(miniscope(a), miniscope(b))
    case Or(a, b) => Or(miniscope(a), miniscope(b))
    case _ => f
  }

  def negIn(f: E): E = f match {
    case Neg(And(a, b)) => Or(negIn(Neg(a)), negIn(Neg(b)))
    case Neg(Or(a, b)) => And(negIn(Neg(a)), negIn(Neg(b)))
    case Neg(Ex(v, t, g)) => All(v, t, negIn(Neg(g)))
    case Neg(All(v, t, g)) => Ex(v, t, negIn(Neg(g)))
    case Neg(Neg(g)) => negIn(g)
    case And(a, b) => And(negIn(a), negIn(b))
    case Or(a, b) => Or(negIn(a), negIn(b))
    case All(v, t, g) => All(v, t, negIn(g))
    case Ex(v, t, g) => Ex(v, t, negIn(g))
    case _ => f
  }

  def skolem(f: E): E = {
    def skolemRec(h: E, boundVars: Set[Var], toFun: Map[Var, E]): E = h match {
      case Or(a, b) =>
        val na = skolemRec(a, boundVars, toFun)
        val nb = skolemRec(b, boundVars, toFun)
        Or(na, nb)
      case And(a, b) =>
        val na = skolemRec(a, boundVars, toFun)
        val nb = skolemRec(b, boundVars, toFun)
        And(na, nb)
      case Neg(a) =>
        val na = skolemRec(a, boundVars, toFun)
        Neg(na)
      case All(v, t, a) =>
        val na = skolemRec(a, boundVars + v, toFun)
        All(v, t, na)
      case Ex(v, t, a) =>
        // TODO: can be collision with original names from input
        val fun = FunctionTerm("skolemize" + indexSkolem, boundVars.toList)
        indexSkolem += 1
        skolemRec(a, boundVars, toFun + (v -> fun))
      case Atom(g, args) =>
        Atom(g, args.toList.map(substitute(_, toFun)))
    }

    skolemRec(f, Set[Var](), Map[Var, E]())
  }

  def removeForAll(f: E): E = {
    def rec(h: E): E = h match {
      case Or(a, b) =>
        Or(rec(a), rec(b))
      case And(a, b) =>
        And(rec(a), rec(b))
      case Neg(a) =>
        Neg(rec(a))
      case All(v, t, a) =>
        rec(a)
      case term@AppRec(g, _) =>
        term
    }

    rec(f)
  }

  def toCNF(f: E, clauseType: ClauseType): CNF = {
    def toCNFRec(h: E): ListBuffer[Clause] = h match {
      case And(a, b) =>
        val l1 = toCNFRec(a)
        val l2 = toCNFRec(b)
        l1 ++ l2
      case Or(a, b) =>
        // TODO: can be collision with original names from input
        val z = AppRec(Sym("predicate" + indexPredicate), a.freeVariables ++ b.freeVariables)
        indexPredicate += 1
        val l1 = toCNFRec(a)
        val l2 = toCNFRec(b)
        l1.map(_ + z) ++ l2.map(_.+:(z))
      case Neg(a) =>
        val l = toCNFRec(a)
        l.map((c: Clause) => new Clause(c.suc, c.ant, clauseType))
      case AppRec(g, args) =>
        ListBuffer[Clause](Clause(clauseType)()(AppRec(g, args)))
    }

    CNF(toCNFRec(f))
  }

  def substitute(term: E, toFun: Map[Var, E]): E = term match {
    case Var(name) => toFun.get(Var(name)) match {
      case Some(fun) => fun
      case None => term
    }
    case Sym(_) => term
    case AppRec(f, args) => AppRec(f, args.map(substitute(_, toFun)))
  }
}

