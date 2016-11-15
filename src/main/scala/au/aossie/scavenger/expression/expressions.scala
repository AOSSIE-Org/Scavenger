package au.aossie.scavenger.expression

import au.aossie.scavenger.util.unicode._
  
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
}

case class Sym(val name: String) extends E {
  def logicalSize = 1
  override def toString = name
}

case class Abs(val variable: Sym, val t: T, val body: E) extends E {
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
  def apply(vars : Iterable[(Sym,T)], body : E) : E =
    vars match {
      case Nil => body
      case (x,t) :: xts => Abs(x,t,apply(xts,body))
    }
  def unapply(e : E): Option[(List[(Sym,T)],E)] = e match {
    case e : Abs => Some(unapplyRec(e))
    case _       => None
  }
  private def unapplyRec(a : Abs) : (List[(Sym,T)],E) = a match {
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
        return (function, firstArgs ++ (e.argument::Nil))
    }
    case _ => return (e.function, e.argument::Nil) 
  } 
}



trait Infix extends Sym