package at.logic.skeptik.algorithm.compressor.FOSplit.equalities

import at.logic.skeptik.expression.formula.Atom
import at.logic.skeptik.expression.{E, Var}

/**
  * This equality is used when we consider that two literals are equal if they have the same name
  * For example, the following literals would be considered equal: P(X), P(a), P(b). (X is a variable,
  * a and b are constante). P is the "name" of the three literals
  */
trait NameEquality {
  def equalLiterals(selectedLiteral: E, nodeLiteral: E): Boolean =
    (selectedLiteral, nodeLiteral) match {
      case (Atom(Var(name1,_), _), Atom(Var(name2,_), _)) => name1 == name2
      case (Atom(Var(name1,_), _),          _           ) => false
      case _                                              => throw new Exception("The literal is not an instance of an Atom\nLiterals: " + selectedLiteral.toString + ", " + nodeLiteral.toString)
    }
}
