package org.aossie.scavenger.parser

import ammonite.ops._
import org.aossie.scavenger.structure.immutable.CNF

/**
  * @author Daniyar Itegulov
  */
trait Parser {
  def parse(filename: Path): CNF
}

