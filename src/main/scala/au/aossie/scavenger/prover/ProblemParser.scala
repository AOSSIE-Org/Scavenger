package au.aossie.scavenger.prover

/**
  * @author Daniyar Itegulov
  */
trait ProblemParser {
  def parse(filename: String): CNF
}
