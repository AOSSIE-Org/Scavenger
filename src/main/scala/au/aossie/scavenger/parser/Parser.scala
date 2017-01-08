package au.aossie.scavenger.parser

import ammonite.ops._
import au.aossie.scavenger.structure.immutable.CNF

/**
  * @author Daniyar Itegulov
  */
trait Parser {
  def parse(filename: Path): CNF
}

