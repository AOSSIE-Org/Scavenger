package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.CNF

/**
  * @author Daniyar Itegulov
  */
trait ProblemParser {
  def parse(filename: String): CNF
}
