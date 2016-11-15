package au.aossie.scavenger.prover

import au.aossie.scavenger.proof.cr.{CRProof => Proof}
import au.aossie.scavenger.model.Model


/**
 * ProblemStatus is the class for results returned by the theorem prover.
 * 
 * The semantics of each subclass is explained in the SZS Ontology:
 * http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology
 * 
 * @author Bruno Woltzenlogel Paleo
 */
abstract sealed class ProblemStatus

abstract class Success extends ProblemStatus 

case class Unsatisfiable(p: Option[Proof] = None) extends Success
case class Satisfiable(m: Option[Model] = None) extends NoSuccess

abstract class NoSuccess extends ProblemStatus 

case object Timeout extends NoSuccess
case object GaveUp extends NoSuccess
case object Error extends NoSuccess