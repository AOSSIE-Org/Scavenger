package au.aossie.scavenger.algorithm.prover.actors.messages

import au.aossie.scavenger.algorithm.prover.structure.immutable.Literal

/**
  * @author Daniyar Itegulov
  */
case class Unify(left: Seq[Literal], right: Seq[Literal])
