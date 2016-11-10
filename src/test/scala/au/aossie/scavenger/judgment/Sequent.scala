package au.aossie.scavenger.judgment

import au.aossie.scavenger.expression._
import au.aossie.scavenger.util.unicode._

import au.aossie.scavenger.expression.position.{PredicatePosition, IndexedPosition}

import org.junit.runner.RunWith
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SequentSpec extends SpecificationWithJUnit {

  val a = Sym("a", o)
  val x = Sym("x", o)
  val s = immutable.Clause(a,x)(a,x,a,x)
  
  "Sequent" should {
    "give a nicely formated string when toString is called" in {
      s.toString must beEqualTo( "a, x" + unicodeOrElse(" \u22A2 "," :- ") + "a, x, a, x")
    }
  }
}
