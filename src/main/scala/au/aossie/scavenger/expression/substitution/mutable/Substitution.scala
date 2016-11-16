package au.aossie.scavenger.expression
package substitution
package mutable

import collection.mutable.{Map => MMap, MapLike => MMapLike}
import collection.mutable.{MapBuilder, Builder}
import collection.generic.CanBuildFrom

final class Substitution
extends AbstractSubstitution with MMap[Sym, E] with MMapLike[Sym, E, Substitution] {
  override protected def m = mm.toMap
  private val mm = MMap[Sym, E]()

  def toImmutable = new immutable.Substitution(mm.toMap)

  def get(key: Sym) = mm.get(key)
  override def update(key: Sym, e: E) = mm.update(key, e)
  override def remove(key: Sym): Option[E] = mm.remove(key)
  def iterator: Iterator[(Sym, E)] = mm.iterator
  def += (kv: (Sym, E)): this.type = { update(kv._1, kv._2); this }
  def -= (key: Sym): this.type  = { remove(key); this }
  override def empty = new Substitution
  override def stringPrefix = "Substitution"
}
object Substitution extends {
  def empty = new Substitution

  def apply(kvs: (Sym, E)*): Substitution = { val s = empty; for (kv <- kvs) s += kv ; s }

  def newBuilder: Builder[(Sym, E), Substitution] = new MapBuilder[Sym, E, Substitution](empty)

  implicit def canBuildFrom: CanBuildFrom[Substitution, (Sym, E), Substitution] =
      new CanBuildFrom[Substitution, (Sym, E), Substitution] {
        def apply(from: Substitution) = newBuilder
        def apply() = newBuilder
      }
}
