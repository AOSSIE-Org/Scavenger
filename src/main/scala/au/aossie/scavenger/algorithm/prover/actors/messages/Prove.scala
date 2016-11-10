package au.aossie.scavenger.algorithm.prover.actors.messages

import au.aossie.scavenger.algorithm.prover.structure.immutable.CNF
import au.aossie.scavenger.expression.Var

import scala.collection.mutable

/**
  * @author Daniyar Itegulov
  */
case class Prove(cnf: CNF, variables: mutable.Set[Var])
