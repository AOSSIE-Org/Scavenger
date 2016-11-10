package au.aossie.scavenger.prover

import au.aossie.scavenger.structure.immutable.Literal
import au.aossie.scavenger.expression.Sym
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.proof.Proof
import au.aossie.scavenger.proof.sequent.SequentProofNode
import au.aossie.scavenger.proof.sequent.conflictresolution._
import au.aossie.scavenger.proof.sequent.lk._

import au.aossie.scavenger.structure.immutable.CNF
import au.aossie.scavenger.structure.immutable.Clause

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

/**
  * @author Daniyar Itegulov
  */
object CR extends Prover {

  def prove(cnf: CNF)(implicit variables: mutable.Set[Sym]): Option[Proof[SequentProofNode]] = {

    val depthLiterals = mutable.Map.empty[Int, mutable.Set[Literal]] // Shows literals that were propagated at this depth
    depthLiterals(0) = mutable.Set.empty
    val ancestor = mutable.Map.empty[Literal, mutable.Set[Clause]] // For each literal what initial clauses produced it
    val implicationGraph = mutable.Map.empty[Clause, ArrayBuffer[Literal]] // For each clause (or literal) what literals was produced from it
    val reverseImplicationGraph = mutable.Map.empty[Literal, mutable.Set[(Clause, Seq[(Literal, Substitution)])]] // For each literal what clause, literals and mgu were used to produce it
    val unifiableUnits = mutable.Map.empty[Literal, mutable.Set[Literal]] // Shows unifiable literals for each literal
    val literals = mutable.Set(cnf.clauses.flatMap(_.literals): _*) // All literals contained in propagated literals and initial clauses
    val propagatedLiterals = mutable.Set.empty[Literal]
    var depth = 0 // Current depth
    val decision = ArrayBuffer.empty[Literal] // Literals, which were decided
    val conflictClauses: mutable.Set[Clause] = mutable.Set.empty[Clause]
    val clauseProof = mutable.Map.empty[Clause, SequentProofNode] // Stores axioms for initial clauses and conflict proof for cdcl clauses

    cnf.clauses.foreach(clause => clauseProof(clause) = Axiom(clause.toSeqSequent))

    def allClauses: Seq[Clause] = cnf.clauses ++ conflictClauses

    // Initial unit-clauses are considered as propagated
    {
      val unitClauses = allClauses.filter(_.isUnit).map(_.literal)
      propagatedLiterals ++= unitClauses
      unitClauses.foreach(literal => ancestor.getOrElseUpdate(literal, mutable.Set.empty) += literal.toClause)
    }

    /*
     * Filling in the unifiableUnits structure: if there is some unit clauses from initial clauses, then we
     * can use them to propagate something at step 1.
     */
    literals.foreach(unifiableUnits(_) = mutable.Set.empty)
    for (literal <- literals) {
      for (other <- allClauses) if (other.isUnit && other.literal.negated != literal.negated) {
        unifyWithRename(Seq(literal.unit), Seq(other.literal.unit)) match {
          case Some(_) => unifiableUnits(literal) += other.literal
          case None =>
        }
      }
    }

    /**
      * Try to resolve given non-unit clause with some propagated literals.
      *
      * @param clause initial non-unit clause
      * @return Set of literals, which were propagated from given clause
      */
    def resolve(clause: Clause): Set[Literal] = {
      val result = mutable.Set.empty[Literal]
      // For each literal in clause we fetch what unit clauses exists which can be resolved with this literal
      // e.g. unifyCandidates for Clause(!P(X), Q(a)) can be Seq(Seq(P(a), P(b), P(f(a))), Seq(!Q(X), !Q(a)))
      val unifyCandidates = clause.literals.map(unifiableUnits.getOrElseUpdate(_, mutable.Set.empty).toSeq)
      for (conclusionId <- unifyCandidates.indices) {
        // Id of literal which will be a conclusion
        val conclusion = clause.literals(conclusionId)
        // All unifiers excluding that one for conclusion
        val unifiers = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
        // All literals excluding conclusion
        val literals = clause.literals.take(conclusionId) ++ clause.literals.drop(conclusionId + 1)
        for (unifier <- combinations(unifiers)) { // We should try all combinations of unifiers
          val unifierUnits = unifier.map(_.unit)
          val literalUnits = literals.map(_.unit)
          unifyWithRename(unifierUnits, literalUnits) match {
            // All unifiers should be unified with literals using one common mgu
            case Some((leftMgu, rightMgu)) =>
              val newLiteral = Literal(rightMgu(conclusion.unit), conclusion.negated)
              unifier.foreach(implicationGraph.getOrElseUpdate(_, ArrayBuffer.empty) += newLiteral)
              if (!unifier.exists(isAncestor(_, newLiteral))) {
                reverseImplicationGraph.getOrElseUpdate(newLiteral, mutable.Set.empty) += ((clause, unifier zip leftMgu))
                ancestor.getOrElseUpdate(newLiteral, mutable.Set.empty) ++=
                  (Set.empty[Clause] /: unifier) (_ union ancestor(_)) + clause
                if (decision.contains(newLiteral)) {
                  decision -= newLiteral
                  result += newLiteral
                }
                if (!result.contains(newLiteral) && !propagatedLiterals.contains(newLiteral)) {
                  result += newLiteral
                }
              }
            case None =>
          }
        }
      }
      result.toSet
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
      } else if (decision contains current) {
        false
      } else {
        reverseImplicationGraph(current).exists {
          case (clause, unifiers) => unifiers.exists { case (lit, _) => isAncestor(lit, ancestor) }
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
      result.foreach(unifiableUnits(_) = mutable.Set.empty)
      for (literal <- literals) {
        for (other <- result) if (other.negated != literal.negated) {
          unifyWithRename(Seq(literal.unit), Seq(other.unit)) match {
            case Some(_) => unifiableUnits(literal) += other
            case None =>
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
    def reset(newClauses: Traversable[Clause]): Unit = {
      conflictClauses ++= newClauses
      depth = 0
      depthLiterals.clear()
      depthLiterals(0) = mutable.Set.empty
      ancestor.clear()
      unifiableUnits.clear()
      literals.clear()
      literals ++= allClauses.flatMap(_.literals)
      propagatedLiterals.clear()
      decision.clear()
      reverseImplicationGraph.clear()

      literals.foreach(unifiableUnits(_) = mutable.Set.empty)
      for (literal <- literals) {
        for (other <- allClauses) if (other.isUnit && other.literal.negated != literal.negated) {
          unifyWithRename(Seq(literal.unit), Seq(other.literal.unit)) match {
            case Some(_) => unifiableUnits(literal) += other.literal
            case None =>
          }
        }
      }

      {
        val unitClauses = allClauses.filter(_.isUnit).map(_.literal)
        propagatedLiterals ++= unitClauses
        unitClauses.foreach(literal => ancestor.getOrElseUpdate(literal, mutable.Set.empty) += literal.toClause)
      }
    }

    /**
      * Finds conflict clause, used consisting of negation of all decisions-ancestors of `current`.
      *
      * @param current literal
      * @param substitution last instantiation of this literal
      * @return clause, representing disjunction of negated decision literals, used in propagation of current literal
      */
    def findConflictClause(current: Literal, substitution: Substitution = Substitution.empty): Clause = {
      if (allClauses contains current.toClause) {
        Clause.empty
      } else if (decision contains current) {
        !substitution(current)
      } else if (reverseImplicationGraph contains current) {
        val conflictClauses = for ((clause, unifier) <- reverseImplicationGraph(current))
          yield (clause, unifier,
              unifier.map {
                case (lit, mgu) => findConflictClause(lit, mgu(substitution))
              }.fold(Clause.empty)(_ union _)
            )
        val (bestClause, bestUnifier, bestConflictClause) = conflictClauses.toSeq.sortBy(_._3.width).head
        reverseImplicationGraph(current).clear()
        reverseImplicationGraph(current) += ((bestClause, bestUnifier))
        bestConflictClause
      } else {
        throw new IllegalStateException("Literal was propagated, but there is no history in implication graph")
      }
    }

    /**
      * Creates formal proof, which formally reasons `current` literal.
      *
      * @param current literal to be proved
      * @return formal proof, which conclusion is the `current`
      */
    def buildProof(current: Literal): SequentProofNode = {
      if (allClauses contains current.toClause) {
        Axiom(current.toClause.toSeqSequent)
      } else if (decision contains current) {
        Decision(current)
      } else if (reverseImplicationGraph contains current) {
        val (clause, unifier) = reverseImplicationGraph(current).head
        val premiseProofs = unifier.map {
          case (lit, _) => buildProof(lit)
        }
        UnitPropagationResolution(premiseProofs, clauseProof(clause), current)
      } else {
        throw new IllegalStateException("Literal was propagated, but there is no history in implication graph")
      }
    }

    while (true) {
      val result = ArrayBuffer.empty[Literal] // New literals, which are propagated on this step
      for (clause <- allClauses if !clause.isUnit) {
        result ++= resolve(clause) // Try to resolve all clause with some other clauses
      }

      // If there is a literal in clause, which is contained either in `clauses` or `result`
      def satisfied = cnf.clauses.filter(_.literals.exists(lit => (propagatedLiterals contains lit) || (result contains lit)))

      def usedAncestors = result.map(ancestor(_)).fold(mutable.Set.empty)(_ union _) ++ satisfied
      def notUsedAncestors = Random.shuffle((cnf.clauses.toSet diff usedAncestors).toSeq)
      while (notUsedAncestors.nonEmpty) {
        // If at least one ancestor wasn't used
        val clause = notUsedAncestors.head
        // We are trying to unify `clause`, so BFS-resolution proceed with this clause.
        // So we retrieve all possible candidates for unifying of each literal and also add this
        // negated literal (we will make appropriate decision to justify this) so we can always choose
        // at least one candidate for resolution.
        val decisionLiteral = Random.shuffle(clause.literals).head
        decision += decisionLiteral
        ancestor(decisionLiteral) = mutable.Set.empty
        depthLiterals(depth) += decisionLiteral
        updateSystem(Seq(decisionLiteral))

        for (ancestor <- notUsedAncestors) {
          result ++= resolve(ancestor)
        }
      }
      println(s"Decided $decision and resolved $result")
      updateSystem(result)
      depth += 1
      depthLiterals(depth) = mutable.Set(result: _*)

      val conflictLearnedClauses = ArrayBuffer.empty[Clause]
      propagatedLiterals.filter(unifiableUnits(_).nonEmpty).foreach { conflictLiteral =>
        // For each literal, which can be unified with some other literal
        val otherLiteral = unifiableUnits(conflictLiteral).head
        println(s"There is a conflict from $conflictLiteral and $otherLiteral")

        val (Seq(leftMgu), rightMgu) = unifyWithRename(Seq(conflictLiteral.unit), Seq(otherLiteral.unit)).get
        val conflictClauseLeft = findConflictClause(conflictLiteral, leftMgu)
        val conflictClauseRight = findConflictClause(otherLiteral, rightMgu)

        println(s"Conflict clause from $conflictLiteral is $conflictClauseLeft")
        println(s"Conflict clause from $otherLiteral is $conflictClauseRight")
        val newClause = unique(conflictClauseLeft union conflictClauseRight)
        val conflict = Conflict(buildProof(conflictLiteral), buildProof(otherLiteral))
        clauseProof(newClause) = ConflictDrivenClauseLearning(conflict)
        println(s"Derived $newClause")
        if (newClause == Clause.empty) return Some(Proof(conflict))
        conflictLearnedClauses += newClause
      }

      if (conflictLearnedClauses.nonEmpty) {
        reset(conflictLearnedClauses)
      } else if (result.isEmpty) {
        return None
      }
    }
    None
  }
}
