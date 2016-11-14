package au.aossie.scavenger.expression

import au.aossie.scavenger.util.unicode._

abstract class T {
  def ->(t:T) = Arrow(this,t)
  def logicalSize: Int
}
trait Atomic {
  def logicalSize = 1
}
case object i extends T with Atomic
case object o extends T with Atomic

// Instructions for overriding the apply method of a case class's companion object:
// http://stackoverflow.com/questions/5827510/how-to-override-apply-in-a-case-class-companion

abstract case class AtomicType private[AtomicType] (name: String) extends T with Atomic {
  override def toString = name
  
  private def readResolve(): Object = AtomicType.apply(name)
  def copy(name:String = name) = AtomicType.apply(name)
}
object AtomicType {
  private[this] val cache = collection.mutable.Map[String, AtomicType]()
  
  def apply(name:String): AtomicType = cache.getOrElseUpdate(name, new AtomicType(name) {} )
}

abstract case class Arrow private[Arrow] (t1:T, t2:T) extends T {
  override def toString = "(" + t1 + unicodeOrElse("\u2192","->") + t2 + ")"
  def logicalSize = t1.logicalSize + t2.logicalSize + 1
  
  private def readResolve(): Object = Arrow.apply(t1, t2)
  def copy(t1:T = t1, t2:T = t2) = Arrow.apply(t1, t2)
}
object Arrow {
  private[this] val cache = collection.mutable.Map[(T,T), Arrow]()
  
  def apply(t1:T, t2:T): Arrow = cache.getOrElseUpdate((t1,t2), new Arrow(t1, t2) {} )
}
