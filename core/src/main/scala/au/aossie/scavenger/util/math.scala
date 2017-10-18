package au.aossie.scavenger.util

package object math {
  def argMax[A](s: Traversable[A], size: A => Int): Option[A] =
    genericMinMax(s, size, Int.MinValue, _ > _)._1
  def argMin[A](s: Traversable[A], size: A => Int): Option[A] =
    genericMinMax(s, size, Int.MaxValue, _ < _)._1

  def max[A](s: Traversable[A], size: A => Int, default: Int = Int.MinValue): Int =
    genericMinMax(s, size, default, _ > _)._2
  def min[A](s: Traversable[A], size: A => Int, default: Int = Int.MaxValue): Int =
    genericMinMax(s, size, default, _ < _)._2

  def genericMinMax[A](s: Traversable[A],
                       size: A => Int,
                       default: Int,
                       compare: (Int, Int) => Boolean): (Option[A], Int) = {
    def rec(t: List[A]): (Option[A], Int) = t match {
      case Nil => (None, default)
      case h :: tail =>
        val r @ (_, max) = rec(tail)
        val hSize        = size(h)
        if (compare(hSize, max)) (Some(h), hSize) else r
    }
    rec(s.toList)
  }
}
