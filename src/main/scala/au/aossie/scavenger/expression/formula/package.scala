package au.aossie.scavenger.expression
package object formula {
  import au.aossie.scavenger.util.unicode._
  import language.implicitConversions

  // Logical Symbols and Connectives

  def booleanFunctionType(arity: Int): T = if (arity == 0) o
                                           else (o -> booleanFunctionType(arity - 1))

  class BigConnective(symbol: String) {
    def apply(arity: Int) = if (arity == 2) new Sym(symbol) with Infix
                            else new Sym(symbol)
  }

  val andS = unicodeOrElse("\u2227","&") // "∧"
  val andC = new Sym(andS) with Infix
  val bigAndC = new BigConnective(andS)

  val orS = unicodeOrElse("\u2228","|")
  val orC = new Sym(orS) with Infix
  val bigOrC = new BigConnective(orS)

  val impS = unicodeOrElse("\u2192","->")
  val impC = new Sym(impS) with Infix

  val allS = unicodeOrElse("\u2200","A")
  def allC = Sym(allS)

  val exS = unicodeOrElse("\u2203","E")
  def exC = Sym(exS)

  val negS = unicodeOrElse("\u00AC","-")
  val negC = Sym(negS)

  val eqS = "="
  def eqC = new Sym(eqS) with Infix

  val equivS = "<=>"
  def equivC = new Sym(equivS) with Infix

  val conditionalConnectiveS = "conditionalFormula"
  val conditionalConnectiveC = new Sym(conditionalConnectiveS)

  def isLogicalConnective(c:E) = c match {
    case Sym(n) =>  n == andS || n == orS || n == impS ||
                      n == allS || n == exS || n == negS ||
                      n == equivS || n == conditionalConnectiveS
    case _ => false
  }


  //implicit def enrichFormula(e: E) = new RichFormula(e)
  implicit class RichFormula (val e: E) extends AnyVal {
    def implies(that: E) = Imp(e, that)
    def →(that: E) = implies(that)

    def and(that: E) = And(e, that)
    def ∧(that: E) = and(that)

    def or(that: E) = Or(e, that)
    def ∨(that: E) = or(that)
  }

  // Since Scala accepts only !, ~, + and - as prefix unary operators,
  // the following methods cannot be implemented in RichFormula

  def neg(f: E) = Neg(f)
  def ¬(f: E) = neg(f)

  def all(v:Sym, t:T) = (f:E) => All(v,t,f)
  def ∀(v:Sym,t:T) = all(v,t)

  def ex(v:Sym, t:T) = (f:E) => Ex(v,t,f)
  def ∃(v:Sym,t:T) = all(v,t)

  def bigOr(args: Iterable[E]) = AppRec(bigOrC(args.size), args)
  def bigAnd(args: Iterable[E]) = AppRec(bigAndC(args.size), args)
}