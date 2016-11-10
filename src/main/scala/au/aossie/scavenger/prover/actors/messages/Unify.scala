package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.Literal

/**
  * @author Daniyar Itegulov
  */
case class Unify(left: Seq[Literal], right: Seq[Literal])
