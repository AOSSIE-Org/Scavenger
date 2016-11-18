package au.aossie.scavenger

import org.scalameter.api._

import au.aossie.scavenger.expression._


/**
  * This class finds out the difference in performance 
  * between using a trait or a hashmap to find out whether
  * a Sym is a unifiable variable.
  *
  */
object TraitVersusMap extends Bench.OfflineRegressionReport {

  trait Var extends Sym
  
  val range = Gen.range("range")(500000,1000000,500000)
  
  performance of "Trait" in {
    measure method "trait" in {
      using(range) in { r =>
        
        val vars = for (i <- 1 to r) yield {
          new Sym("s" + i) with Var
        }
        
        for (v <- vars) {
          v match {
            case x: Var => x.name.drop(1).toInt + 1
          }
        }        
      }
    }

  }
  
  performance of "HashMap" in {
    measure method "HashMap" in {
      using(range) in { r =>
        val m = new collection.mutable.HashSet[Sym]()
        val vars = for (i <- 1 to r) yield {
          val v = new Sym("s" + i)
          m += v
          v
        }
        
        for (v <- vars) {
          if (m contains v) v.name.drop(1).toInt + 1
        }
      }
    }
  }
  
}

