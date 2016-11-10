package au.aossie.scavenger.prover.actors.messages

import au.aossie.scavenger.structure.immutable.CNF
import au.aossie.scavenger.expression.Var

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
case class Prove(cnf: CNF, variables: mutable.Set[Var])
