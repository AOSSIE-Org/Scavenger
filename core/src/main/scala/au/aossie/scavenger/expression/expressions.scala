package au.aossie.scavenger.expression

import au.aossie.scavenger.util.unicode._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashSet}

sealed abstract class E {
  def logicalSize: Int

  //alphaEquals
  def =+=(that:E) = {
    def rec(e1:E,e2:E,map:Map[Sym,Sym],typeContext1:Map[Sym,Option[T]],typeContext2:Map[Sym,Option[T]]): Boolean = (e1,e2) match {
      case (v1:Sym, v2:Sym) => map.getOrElse(v1,v1)==v2 && typeContext1.getOrElse(v1, None) == typeContext2.getOrElse(v2, None)
      case (Abs(v1,t1,b1),Abs(v2,t2,b2)) => {
        if (v1 == v2 && t1 == t2) rec(b1, b2, map, typeContext1.updated(v1, Some(t1)), typeContext2.updated(v2, Some(t2)))
        else if (t1 == t2) rec(b1, b2, map.updated(v1,v2), typeContext1.updated(v1, Some(t1)), typeContext2.updated(v2, Some(t2)))
        else false
      }
      case (App(f1,a1),App(f2,a2)) => rec(f1, f2, map, typeContext1, typeContext2) && rec(a1, a2, map, typeContext1, typeContext2)
      case _ => false
    }
    rec(this, that, Map(), Map(), Map())
  }
  def occursIn(e:E):Boolean = if (this == e) true else e match {
    case v: Sym => false
    case App(f,a) => (this occursIn f) || (this occursIn a)
    case Abs(v,t,g) => (this occursIn v) || (this occursIn g)
  }
  
  lazy val variables: Seq[Var] = {
    val result = ArrayBuffer.empty[Var]
    def rec(e: E): Unit = e match {
      case v: Var => result += v
      case App(f, a) =>
        rec(f)
        rec(a)
      case Abs(v, _, b) =>
        rec(v)
        rec(b)
      case _ =>
    }
    rec(this)
    result
  }
  
  lazy val freeVariables = {
    def rec(e: E, bVars: Set[Var]): Seq[Var] = e match {
      case v: Var if !(bVars contains v) => Seq(v)
      case App(f, a) => rec(f, bVars) ++ rec(a, bVars)
      case Abs(v,t,b) => rec(b, bVars + v)
      case _ => Seq()
    }
    rec(this, Set[Var]()).distinct
  }

  lazy val depth = {
    def rec(e: E): Int = e match {
      case Var(_) | Sym(_) => 0
      case AppRec(_, args) =>
        args.map(rec).max + 1
      case Abs(_, _, b) => rec(b) + 1
      case _ => 0
    }
    rec(this)
  }

  lazy val functionSymbols: mutable.HashSet[(Sym, Int)] = {
    def rec(e: E, syms: HashSet[(Sym, Int)]): Unit = e match {
      case Var(_) =>
      case sym@Sym(_) =>
        syms.add((sym, 0))
      case AppRec(f: Sym, args) =>
        syms.add((f, args.size))
        args.foreach(rec(_, syms))
    }
    val syms = HashSet.empty[(Sym, Int)]
    rec(this, syms)
    syms
  }
}

case class Sym(val name: String) extends E {
  def logicalSize = 1
  override def toString = name
}

trait Var extends Sym
object Var {
  def apply(name: String) = new Sym(name) with Var
  def unapply(e: E): Option[String] = e match {
    case v: Var => Some(v.name)
    case _ => None
  }
}

case class Abs(val variable: Var, val t: T, val body: E) extends E {
  def logicalSize = (1 + t.logicalSize + 1) + body.logicalSize + 1
  override def toString = unicodeOrElse("\u03BB","@") + variable.name + ":" + t + "." + body
}
case class App(val function: E, val argument: E) extends E {
  def logicalSize = function.logicalSize + argument.logicalSize + 1
  override def toString = this match {
    case App(App(s:Sym with Infix, a), b) => "(" + a + " " + s + " " + b +  ")"
    case AppRec(f, args) => "(" + f + " " + args.mkString(" ") + ")"
  }
}

object AbsRec {
  def apply(vars: Iterable[(Var,T)], body : E) : E =
    vars match {
      case Nil => body
      case (x,t) :: xts => Abs(x,t,apply(xts,body))
    }
  def unapply(e: E): Option[(List[(Var,T)],E)] = e match {
    case e: Abs => Some(unapplyRec(e))
    case _       => None
  }
  private def unapplyRec(a: Abs) : (List[(Var,T)],E) = a match {
    case Abs(x,t1,body) => body match {
      case Abs(y,t2,b) =>
        val (vars,b2) = unapplyRec(Abs(y,t2,b))
        ((x,t1)::vars,b2)
      case _        => (List((x,t1)),body)
    }
  }
}

object AppRec {
  def apply(p: E, args: Iterable[E]) = (p /: args)((p,a) => App(p,a))
  def unapply(e:E) = e match {
    case e: Sym => Some((e,Nil))
    case e: App => Some(unapplyRec(e))
    case _ => None
  }
  private def unapplyRec(e: App): (E,Iterable[E]) = e.function match {
    case a : App => {
      val (function, firstArgs) = unapplyRec(a)
      (function, firstArgs ++ (e.argument::Nil))
    }
    case _ => (e.function, e.argument::Nil)
  }
}

trait Infix extends Sym
