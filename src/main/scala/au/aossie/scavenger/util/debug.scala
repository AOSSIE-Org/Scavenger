package au.aossie.scavenger.util

object debug {
  def debug(s: Any)(implicit i:Int) = {
    println(((1 to i).toList.map(x => "    ") :\ "")(_+_) + s)
  }
}