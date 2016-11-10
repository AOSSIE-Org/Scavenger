package au.aossie.scavenger.expression

class RichString protected[expression] (s: String) {
  def ^(t: T) = Var(s, t) 
}

