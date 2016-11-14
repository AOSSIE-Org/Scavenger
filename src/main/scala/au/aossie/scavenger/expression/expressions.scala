package au.aossie.scavenger.expression

import au.aossie.scavenger.util.unicode._
  
// TODO: implement sharing of expressions

sealed abstract class E {
  def t: T
    
  def logicalSize: Int
  
  //alphaEquals
  def =+=(that:E) = {
    def rec(e1:E,e2:E,map:Map[Sym,Sym]): Boolean = (e1,e2) match {
      case (v1:Sym, v2:Sym) => map.getOrElse(v1,v1)==v2
      case (Abs(v1@Sym(_,t1),b1),Abs(v2@Sym(_,t2),b2)) => {
        if (v1 == v2) rec(b1, b2, map)
        else if (t1 == t2) rec(b1, b2, map.updated(v1,v2))
        else false
      }
      case (App(f1,a1),App(f2,a2)) => rec(f1, f2, map) && rec(a1, a2, map)
      case _ => false
    }
    rec(this, that, Map())
  }
  def occursIn(e:E):Boolean = if (this == e) true else e match {
    case v: Sym => false
    case App(f,a) => (this occursIn f) || (this occursIn a)
    case Abs(v,g) => (this occursIn v) || (this occursIn g)
  }
}
// TODO: Remove type to gain efficiency
case class Sym(val name: String, override val t:T) extends E {
  def logicalSize = 1
  override def toString = name
}

case class SymB(val name: String) extends E {
  
}

case class Abs(val variable: Sym, val body: E) extends E {
  override lazy val t = variable.t -> body.t 
  def logicalSize = (variable.t.logicalSize + 1) + body.logicalSize + 1
  override def toString = unicodeOrElse("\u03BB","@") + variable.name + ":" + variable.t + "." + body
}
case class App(val function: E, val argument: E) extends E {
  require(function.t.asInstanceOf[Arrow].t1 == argument.t)
  override lazy val t = function.t.asInstanceOf[Arrow].t2
  def logicalSize = function.logicalSize + argument.logicalSize + 1
  override def toString = this match {
    case App(App(s:Sym with Infix, a), b) => "(" + a + " " + s + " " + b +  ")"
    case AppRec(f, args) => "(" + f + " " + args.mkString(" ") + ")"
  } 
}

object AbsRec {
  def apply(vars : Iterable[Sym], body : E) : E =
    vars match {
      case List(x) => Abs(x,body)
      case x :: xs => Abs(x,apply(xs,body))
    }
  def unapply(e : E): Option[(List[Sym],E)] = e match {
    case e : Abs => Some(unapplyRec(e))
    case _       => None
  }
  private def unapplyRec(a : Abs) : (List[Sym],E) = a match {
    case Abs(x,body) => body match {
      case Abs(y,b) =>
        val (vars,b2) = unapplyRec(Abs(y,b))
        (x::vars,b2)
      case _        => (List(x),body)
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