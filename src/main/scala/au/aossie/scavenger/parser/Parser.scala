package au.aossie.scavenger.parser

import au.aossie.scavenger.structure.immutable.CNF

/**
  * @author Daniyar Itegulov
  */
trait Parser {
  def parse(filename: String): CNF
}
