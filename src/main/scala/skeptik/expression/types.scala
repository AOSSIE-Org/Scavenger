package skeptik.expression

sealed abstract class T {
  def ->(t:T) = arrow(this,t)
}
case object i extends T
case object o extends T
final case class arrow(t1:T, t2:T) extends T {
  override def toString = "(" + t1 + "->" + t2 + ")"
}
