package au.aossie.scavenger.expression.term

import au.aossie.scavenger.expression.{AppRec, AtomicType, E, T, Sym, Var, i, o}

/**
  * Objects to facilitate the creation of logical terms.
  *
  * @author Ezequiel Postan
  * @since 24.05.2016
  */
abstract class Symbol {
  def apply(name: String) : E = Sym(name)
  def apply(name: String, i: T): E = Sym(name)
}

object DistinctObjectTerm extends Symbol {
  //override def apply(name : String) :E = apply(name,i)
}

object NumberTerm extends Symbol {
  //override def apply(number : String) :E = apply(number,AtomicType("$int"))
}

object Constant extends Symbol {
  //override def apply(name : String) : E = apply(name,i)
}

object Variable extends Symbol {
  override def apply(name : String) : E = Var(name)
  override def apply(name : String, t: T) : E = Var(name)
}

object FunctionTerm {
  def apply(name : String, args : List[E]) : E = AppRec(Sym(name),args)
  //def apply(name : String,typ : T , args : List[E]) : E = AppRec(Symbol(name,typ),args)
  def unapply(e:E) = e match {
    case AppRec(f,args) if args.nonEmpty => Some((f,args))
    case _ => None
  }
}

object TypedNumberTerm extends Symbol {
  //def apply(number : String, numberType : String) : E = newTerm(number, AtomicType(numberType))
}

object TypedConstant extends Symbol {
  //def apply(name : String, constantType : T) : E = newTerm(name,constantType)
}

object TypedVariable extends Symbol {
  override def apply(name : String) : E = Var(name)
  override def apply(name : String, t: T) : E = Var(name)
}

object TypedFunctionSymbol extends Symbol {
  //def apply(name : String, functionSymbolType : T) : E = Sym(name)
}

object ConditionalTerm {
  val conditional = "conditionalTerm"
  val ifThenElse  = new Sym(conditional)
  def apply(condition : E , t1 : E, t2 : E) : E = {
    AppRec(ifThenElse,List(condition,t1,t2))
  }
}


