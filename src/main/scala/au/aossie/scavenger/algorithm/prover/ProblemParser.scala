package au.aossie.scavenger.algorithm.prover

/**
  * @author Daniyar Itegulov
  */
trait ProblemParser {
  def parse(filename: String): CNF
}
