package au.aossie.scavenger.expression
package substitution
package immutable

import collection.immutable.MapLike
import collection.mutable.{Builder, MapBuilder}
import collection.generic.CanBuildFrom

final class Substitution (override protected val m: Map[Sym, E])
extends AbstractSubstitution with Map[Sym, E] with MapLike[Sym, E, Substitution] {
  def get(key: Sym) = m.get(key)
  def iterator: Iterator[(Sym, E)] = m.iterator
  def + [B >: E](kv: (Sym, B)) = {
    if (kv._2.isInstanceOf[E]) new Substitution(m + kv.asInstanceOf[(Sym,E)])
    else m + kv
  }
  def - (key: Sym)  = new Substitution(m - key)
  override def empty = new Substitution(Map[Sym,E]())
  override def stringPrefix = "Substitution"

  def apply(other: Substitution): Substitution = {
    val composedSubstitution = for ((key, value) <- this) yield (key, other(value))
    new Substitution(composedSubstitution)
  }
}
object Substitution {
  def empty = new Substitution(Map[Sym,E]())
  def apply(kvs: (Sym, E)*): Substitution = new Substitution(Map[Sym,E](kvs:_*))

  def newBuilder: Builder[(Sym, E), Substitution] = new MapBuilder[Sym, E, Substitution](empty)

  implicit def canBuildFrom: CanBuildFrom[Substitution, (Sym, E), Substitution] =
      new CanBuildFrom[Substitution, (Sym, E), Substitution] {
        def apply(from: Substitution) = newBuilder
        def apply() = newBuilder
      }
}
