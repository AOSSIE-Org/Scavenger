package au.aossie.scavenger.model

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.expression.formula._
import au.aossie.scavenger.unification.{MartelliMontanari => unify}

// TODO: write the boilerplate to make this a collection class
class Assignment(trueFacts: Set[E]) extends Model {

  // scalastyle:off cyclomatic.complexity
  def truthValue(e: E): Option[Boolean] = {
    e match {
      case _ if trueFacts.contains(e) => Some(true)
      case Neg(f) =>
        truthValue(f) match {
          case Some(b) => Some(!b)
          case None => None
        }
      case And(f1, f2) =>
        (truthValue(f1), truthValue(f2)) match {
          case (Some(true), Some(true)) => Some(true)
          case (_, None) => None
          case (None, _) => None
          case _ => Some(false)
        }
      case Or(f1, f2) =>
        (truthValue(f1), truthValue(f2)) match {
          case (Some(false), Some(false)) => Some(false)
          case (_, Some(true)) => Some(true)
          case (Some(true), _) => Some(true)
          case _ => None
        }
      case Imp(f1, f2) =>
        (truthValue(f1), truthValue(f2)) match {
          case (Some(true), Some(false)) => Some(false)
          case (_, Some(true)) => Some(true)
          case (Some(false), _) => Some(true)
          case _ => None
        }
      case Equivalence(f1, f2) =>
        (truthValue(f1), truthValue(f2)) match {
          case (Some(false), Some(false)) => Some(true)
          case (Some(true), Some(true)) => Some(true)
          case (Some(false), Some(true)) => Some(false)
          case (Some(true), Some(false)) => Some(false)
          case _ => None
        }
      case All(_, _, _) =>
        None // we could do something better than this if we knew that v is of a finite type.
      case Ex(_, _, f) =>
        if (trueFacts.exists(t => unify(f, t).isDefined)) Some(true) else None
    }
  }
  // scalastyle:on cyclomatic.complexity

  override def toString: String = "Model: " + trueFacts.mkString(", ")
}
