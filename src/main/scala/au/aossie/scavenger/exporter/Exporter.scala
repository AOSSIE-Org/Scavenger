package au.aossie.scavenger.exporter

import au.aossie.scavenger.expression.E
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.cr.{CRProofNode => N}

import au.aossie.scavenger.structure.immutable.Clause
import au.aossie.scavenger.util.io.Output

abstract class Exporter extends Output {
  def write(e: E)
  def write(s: Clause)
  def write(p: Proof[N])
}

