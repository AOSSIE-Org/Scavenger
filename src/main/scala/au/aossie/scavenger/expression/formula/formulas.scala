package au.aossie.scavenger.expression
package formula

abstract class Formula {
  def unapply(f:E):Option[_]
  def ?:(f: E) = unapply(f).isInstanceOf[Some[_]]
}

abstract class BinaryFormula(connective: Sym) extends Formula {
  def apply(): E = connective
  def apply(f1: E, f2: E) = App(App(connective,f1),f2)
  def unapply(e:E) = e match {
    case App(App(c,f1),f2) if c == connective => Some((f1,f2))
    case _ => None
  }  
}

abstract class UnaryFormula(connective: Sym) extends Formula {
  def apply() : E = connective
  def apply(f: E) = App(connective,f)
  def unapply(e:E) = e match {
    case App(c,f) if c == connective => Some(f)
    case _ => None
  }  
}

abstract class QuantifierFormula(quantifier: Sym) extends Formula {
  def apply(v:Sym, t: T, f:E) = App(quantifier, Abs(v,t,f))

  def apply(vars : List[(Sym,T)], f : E) : E = {
    vars match {
      case Nil => f
      case (v,t)::tail => apply(v, t, apply(tail,f))
    }
  }

  def unapply(e:E) = e match {
    case App(q, Abs(v,t,f)) if q == quantifier => Some((v,t,f))
    case _ => None
  }  
}


object True extends Sym("true")

object False extends Sym("false")

object Neg extends UnaryFormula(negC)

object And extends BinaryFormula(andC)

object Or extends BinaryFormula(orC)

object Imp extends BinaryFormula(impC)
  
object Equivalence extends BinaryFormula(equivC)

object All extends QuantifierFormula(allC)  

object Ex extends QuantifierFormula(exC)

object Atom extends Formula {
  def apply(p: E, args: List[E]) = {
    val atom = AppRec(p,args)
    atom
  }
//  def apply(name: String, args: List[E]) = {
//    val p    = Sym(name)
//    val atom = AppRec(p,args)
//    atom
//  }
  def unapply(e:E) = e match {
    case AppRec(f,args) if (!isLogicalConnective(f)) => Some((f,args))
    case _ => None
  }
}

object ConditionalFormula extends Formula{
  def apply(cond : E, f1 : E, f2 : E) : E = AppRec(conditionalConnectiveC,List(cond,f1,f2))
  override def unapply(f: E): Option[_] = f match {
    case AppRec(Sym(n),List(cond,f1,f2)) if n == conditionalConnectiveS => Some((cond,f1,f2))
    case _                                                    => None
  }
}

object FormulaEquality extends BinaryFormula(eqC)

