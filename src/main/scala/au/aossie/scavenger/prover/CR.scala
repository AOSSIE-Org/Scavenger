package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.{ Literal, CNF, SeqClause }
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.cr.{ CRProof => Proof, _ }
import au.aossie.scavenger.model.Assignment

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

/**
  * @author Daniyar Itegulov
  */
object CR extends Prover {

  // TODO: refactor this class and enable scalastyle
  // scalastyle:off
  def prove(cnf: CNF)(implicit variables: mutable.Set[Sym]): ProblemStatus = {

    // TODO: this is a temporal determined random for easier debugging
    val rnd = new Random(132374)

    // For each literal what initial clauses produced it
    val ancestor = mutable.Map.empty[Literal, mutable.Set[SeqClause]]
    // For each clause what proof nodes can be used to derive it
    val reverseImplicationGraph = mutable.Map.empty[SeqClause, ArrayBuffer[CRProofNode]]
    cnf.clauses.foreach(clause => reverseImplicationGraph(clause) = ArrayBuffer(Axiom(clause)))
    // Shows unifiable literals for each literal
    val unifiableUnits = mutable.Map.empty[Literal, mutable.Set[Literal]]
    // All literals contained in propagated literals and initial clauses
    val literals           = mutable.Set(cnf.clauses.flatMap(_.literals): _*)
    val propagatedLiterals = mutable.Set.empty[Literal]
    // Current depth
    var depth = 0
    // Literals, which were decided
    val decisions       = ArrayBuffer.empty[Literal]
    val conflictClauses = mutable.Set.empty[CRProofNode]

    def allClauses: Seq[SeqClause] = cnf.clauses ++ conflictClauses.map(_.conclusion)

    // Initial unit-clauses are considered as propagated
    {
      val unitClauses = allClauses.filter(_.isUnit).map(_.literal)
      propagatedLiterals ++= unitClauses
      unitClauses.foreach { literal =>
        ancestor.getOrElseUpdate(literal, mutable.Set.empty) += literal.toClause
      }
    }

    /*
     * Filling in the unifiableUnits structure: if there is some unit clauses from initial clauses, then we
     * can use them to propagate something at step 1.
     */
    literals.foreach(unifiableUnits.getOrElseUpdate(_, mutable.Set.empty))
    for (literal <- literals) {
      for (other <- allClauses) if (other.isUnit && other.literal.negated != literal.negated) {
        unifyWithRename(Seq(literal.unit), Seq(other.literal.unit)) match {
          case Some(_) => unifiableUnits(literal) += other.literal
          case None    =>
        }
      }
    }

    // TODO: try to avoid nested function definitions when possible (i.e. when they do not depend on local variables). Prefer private defs in the class body instead.
    /**
      * Try to resolve given non-unit clause with some propagated literals.
      *
      * @param clause initial non-unit clause
      * @return Set of literals, which were propagated from given clause
      */
    def resolve(clause: SeqClause, result: mutable.Set[Literal]): Set[Literal] = {
      // For each literal in clause we fetch what unit clauses exists which can be resolved with this literal
      // e.g. unifyCandidates for Clause(!P(X), Q(a)) can be Seq(Seq(P(a), P(b), P(f(a))), Seq(!Q(X), !Q(a)))
      val unifyCandidates =
        clause.literals.map(unifiableUnits.getOrElseUpdate(_, mutable.Set.empty).toSeq)
      val myResult = mutable.Set.empty[Literal]
      val combs    = unifyCandidates.map(_.length).product
      if (combs > 100000) {
        println(s"WARNING! Too many combinations: $combs")
      }
      for (conclusionId <- unifyCandidates.indices) {
        // All unifiers excluding that one for conclusion
        val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
        // All literals excluding conclusion
        val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)
        for (unifier <- combinations(unifiers)) { // We should try all combinations of unifiers
          val unifierUnits = unifier.map(_.unit)
          val literalUnits = literals.map(_.unit)
          unifyWithRename(unifierUnits, literalUnits) match {
            // All unifiers should be unified with literals using one common mgu
            case Some(_) =>
              val clauseNode   = reverseImplicationGraph(clause).head
              val unifierNodes = unifier.map(l => reverseImplicationGraph(l.toSeqSequent).head)
              val unitPropagationNode =
                UnitPropagationResolution(unifierNodes, clauseNode, conclusionId)
              val newLiteral = unitPropagationNode.conclusion.literal
              if (!unifier.exists(isAncestor(_, newLiteral))) {
                ancestor.getOrElseUpdate(newLiteral, mutable.Set.empty) ++=
                  (Set.empty[SeqClause] /: unifier)(_ union ancestor(_)) + clause
                if (decisions.contains(newLiteral)) {
                  decisions -= newLiteral
                  result += newLiteral
                  myResult += newLiteral
                  val buffer =
                    reverseImplicationGraph.getOrElseUpdate(newLiteral, ArrayBuffer.empty)
                  buffer.clear()
                  buffer += unitPropagationNode
                } else {
                  val buffer =
                    reverseImplicationGraph.getOrElseUpdate(newLiteral, ArrayBuffer.empty)
                  buffer += unitPropagationNode
                }
                if (!result.contains(newLiteral) && !propagatedLiterals.contains(newLiteral)) {
                  result += newLiteral
                  myResult += newLiteral
                }
              }
            case None =>
          }
        }
      }
      myResult.toSet
    }

    /**
      * Check if `initial` is an ancestor of `initial` in implication graph.
      * @param current to check as a descendant
      * @param ancestor to check as an ancestor
      * @return true, if initial is ancestor of current
      *         false, otherwise
      */
    def isAncestor(current: Literal, ancestor: Literal): Boolean = {
      if (current == ancestor) return true
      if (allClauses contains current.toClause) {
        false
      } else if (decisions contains current) {
        false
      } else {
        if (!reverseImplicationGraph(current).forall {
              case UnitPropagationResolution(unifiers, _, _, _, _) =>
                unifiers.exists { proofNode =>
                  isAncestor(proofNode.conclusion.literal, ancestor)
                }
              case _ =>
                require(false) // unexpected branch
                false
            }) {
          reverseImplicationGraph(current) = reverseImplicationGraph(current).filterNot {
            case UnitPropagationResolution(unifiers, _, _, _, _) =>
              unifiers.exists { proofNode =>
                isAncestor(proofNode.conclusion.literal, ancestor)
              }
            case _ =>
              false
          }
          false
        } else {
          true
        }
      }
    }

    /**
      * Update system with new propagated literals from `result`.
      *
      * @param result containing new literals
      */
    def updateSystem(result: Traversable[Literal]) = {
      literals ++= result
      propagatedLiterals ++= result
      result.foreach(unifiableUnits.getOrElseUpdate(_, mutable.Set.empty))
      for (literal <- literals) {
        for (other <- result) if (other.negated != literal.negated) {
          unifyWithRename(Seq(literal.unit), Seq(other.unit)) match {
            case Some(_) => unifiableUnits(literal) += other
            case None    =>
          }
        }
      }
    }

    /**
      * Resets system to initial state and add `newClauses` as
      * conflict driven clauses.
      *
      * @param newClauses clauses to be added as conflict
      */
    def reset(newClauses: Traversable[CRProofNode]): Unit = {
      conflictClauses ++= newClauses
      depth = 0
      ancestor.clear()
      unifiableUnits.clear()
      literals.clear()
      literals ++= allClauses.flatMap(_.literals)
      propagatedLiterals.clear()
      decisions.clear()
      reverseImplicationGraph.clear()
      cnf.clauses.foreach(clause => reverseImplicationGraph(clause) = ArrayBuffer(Axiom(clause)))
      conflictClauses.foreach(node => reverseImplicationGraph(node.conclusion) = ArrayBuffer(node))

      literals.foreach(unifiableUnits.getOrElseUpdate(_, mutable.Set.empty))
      for (literal <- literals) {
        for (other <- allClauses) if (other.isUnit && other.literal.negated != literal.negated) {
          unifyWithRename(Seq(literal.unit), Seq(other.literal.unit)) match {
            case Some(_) => unifiableUnits(literal) += other.literal
            case None    =>
          }
        }
      }

      {
        val unitClauses = allClauses.filter(_.isUnit).map(_.literal)
        propagatedLiterals ++= unitClauses
        unitClauses.foreach { literal =>
          ancestor.getOrElseUpdate(literal, mutable.Set.empty) += literal.toClause
        }
      }
    }

    while (true) {
      println(s"Starting level $depth")
      val result = mutable.Set.empty[Literal] // New literals, which are propagated on this step
      for (clause <- allClauses if !clause.isUnit) {
        resolve(clause, result) // Try to resolve all clause with some other clauses
      }

      // If there is a literal in clause, which is contained either in `clauses` or `result`
      def satisfied =
        cnf.clauses.filter(_.literals.exists(lit =>
          (propagatedLiterals contains lit) || (result contains lit)))
      def unitClauses = cnf.clauses.filter(_.literals.size == 1)
      def usedAncestors =
        result.map(ancestor(_)).fold(mutable.Set.empty)(_ union _) ++ satisfied ++ unitClauses
      def notUsedAncestors = rnd.shuffle((cnf.clauses.toSet diff usedAncestors).toSeq)
      val usedDecisions    = mutable.Set.empty[Literal]
      while (notUsedAncestors.nonEmpty) {
        // If at least one ancestor wasn't used
        val clause = notUsedAncestors.head
        // We are trying to unify `clause`, so BFS-resolution proceed with this clause.
        // So we retrieve all possible candidates for unifying of each literal and also add this
        // negated literal (we will make appropriate decision to justify this) so we can always choose
        // at least one candidate for resolution.
        val decisionLiteral = rnd.shuffle(clause.literals).head
        decisions += decisionLiteral
        ancestor(decisionLiteral) = mutable.Set.empty
        require(
          !reverseImplicationGraph.contains(decisionLiteral) || reverseImplicationGraph(
            decisionLiteral).isEmpty)
        reverseImplicationGraph(decisionLiteral) = ArrayBuffer(Decision(decisionLiteral))
        updateSystem(Seq(decisionLiteral))

        for (ancestor <- notUsedAncestors) {
          if (resolve(ancestor, result).nonEmpty) {
            usedDecisions += decisionLiteral
          }
        }
      }
      println(s"Resolved $result")
      updateSystem(result)
      depth += 1

      val interestingConflictLearnedClauses = mutable.Set.empty[CRProofNode]
      val allConflictLearnedClauses         = mutable.Set.empty[CRProofNode]
      propagatedLiterals.filter(unifiableUnits(_).nonEmpty).foreach { conflictLiteral =>
        for {
          otherLiteral <- unifiableUnits(conflictLiteral)
          conflictNode <- reverseImplicationGraph(conflictLiteral)
          otherNode <- reverseImplicationGraph(otherLiteral)
          conflict = Conflict(conflictNode, otherNode)
        } {
          val newClause = conflict.findDecisions(Substitution.empty)
          if (newClause == SeqClause.empty) return Unsatisfiable(Some(Proof(conflict)))
          val cdclNode = ConflictDrivenClauseLearning(conflict)
          if (!allClauses.contains(cdclNode.conclusion)) {
            interestingConflictLearnedClauses += cdclNode
            usedDecisions ++= conflict.listDecisions()
          }
          allConflictLearnedClauses += cdclNode
        }
      }

      println(s"Premature decisions: $decisions")

      if (allConflictLearnedClauses.isEmpty &&
          cnf.clauses.forall(_.literals.exists(propagatedLiterals.contains))) {
        val literals      = propagatedLiterals ++ decisions
        val trueLiterals  = literals.filterNot(_.negated).map(_.unit).toSet
        val falseLiterals = literals.filter(_.negated).map(_.unit).map(x => Neg(x)).toSet
        return Satisfiable(Some(new Assignment(trueLiterals ++ falseLiterals)))
      }

      val notUsedDecisions = decisions -- usedDecisions
      if (notUsedDecisions.nonEmpty) {
        println(s"There are some not used decisions: $notUsedDecisions")
        decisions --= notUsedDecisions

        def valid(node: CRProofNode): Boolean = {
          node match {
            case Decision(literal) if notUsedDecisions.contains(literal) =>
              false
            case Decision(_) =>
              true
            case Axiom(_) =>
              true
            case ConflictDrivenClauseLearning(_) =>
              true
            case UnitPropagationResolution(left, right, _, _, _) =>
              left.forall(valid) && valid(right)
          }
        }

        for (literal <- propagatedLiterals) {
          reverseImplicationGraph(literal) = reverseImplicationGraph(literal).filter(valid)
        }
        val nonValidLiterals = reverseImplicationGraph.filter(_._2.isEmpty).keys.map(_.literal)
        propagatedLiterals --= nonValidLiterals
        literals --= nonValidLiterals
        unifiableUnits.values.foreach(_ --= nonValidLiterals)
      }

      println(s"Decided $decisions")

      if (interestingConflictLearnedClauses.nonEmpty) {
        val cdclClauses = interestingConflictLearnedClauses.map(_.conclusion).map(tptpPrettify)
        println("New CDCL clauses:\n" + cdclClauses.mkString("\n"))
        reset(interestingConflictLearnedClauses)
      }
    }
    Error // this line is unreachable.
  }
  // scalastyle:on
}
