package au.aossie.scavenger.prover

import au.aossie.scavenger.expression._
import au.aossie.scavenger.expression.formula.Neg
import au.aossie.scavenger.expression.substitution.immutable.Substitution
import au.aossie.scavenger.model.Assignment
import au.aossie.scavenger.proof.cr.{CRProof => Proof, _}
import au.aossie.scavenger.structure.immutable.{CNF, Clause, Literal}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random
import au.aossie.scavenger.unification.{MartelliMontanari => unify}
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory


/**
  * @author Daniyar Itegulov
  */
object EPCR extends Prover {

  // TODO: Think about every usage of randomness
  val rnd = new Random(107)

  // TODO: Do research about this constants
  val MAX_CNT_CANDIDATES: Int = 40
  val MAX_CNT_WITHOUT_DECISIONS: Int = 10
  val MAX_PROVED_LITERALS_SIZE: Int = 10000

  // TODO: Bad practice to use predefined name(could be collision)
  val VARIABLE_NAME: String = "___VARIABLE___"

  /**
    * Constants for VSIDS heuristic
    */
  val BUMP: Double = 1.0
  val DECAY_FACTOR: Double = 0.9

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    val logger = Logger(LoggerFactory.getLogger("prover"))


    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(Axiom(Clause.empty))))
    }

    /**
      * Mutable set of proved literals initialized with the input CNF's unit clauses.
      */
    val provedLiterals: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Non-unit clauses from the input CNF plus CDCL clauses.
      */
    var nonUnitClauses: mutable.Set[Clause] = mutable.Set(cnf.clauses.filter(!_.isUnit): _*)

    /**
      * All literals used in `nonUnitClauses`.
      */
    var literals: mutable.Set[Literal] = nonUnitClauses.flatMap(_.literals)(collection.breakOut)

    /**
      * Mutable map showing which value-literals can be unified with the key-literal.
      */
    // TODO: is it necessarily to use Ids and Buff?
    val unifiableUnitsIds: mutable.Map[Literal, Int] = mutable.Map.empty
    val unifiableUnitsBuff: ArrayBuffer[mutable.Set[Literal]] = mutable.ArrayBuffer.empty

    /**
      * Mutable map showing all possible proofs for every proved clause.
      */
    // TODO: is it necessarily to use Ids and Buff?
    val reverseImplication: mutable.Map[Clause, Int] = mutable.Map.empty
    val bufferNodes: ArrayBuffer[ArrayBuffer[CRProofNode]] = mutable.ArrayBuffer.empty

    /**
      * All decisions made at this point.
      */
    val decisions: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Set of clauses proved using CDCL rule.
      */
    val cdclClauses: mutable.Set[CRProofNode] = mutable.Set.empty

    /**
      * VSIDS haracteristic helps to choose decision.
      */
    val activity: mutable.Map[Literal, Double] = mutable.HashMap.empty

    /**
      * Memorization for isValidCheck method.
      */
    val memIsValid: mutable.HashMap[CRProofNode, Boolean] = mutable.HashMap.empty

    /**
      * Memorization for getAllConflictDecisions method.
      */
    val memGetConflictDecisions: mutable.HashSet[CRProofNode] = mutable.HashSet.empty

    // TODO: Do research about to store only part of all unifications.
    def updateUnifiableUnits(newLiterals: Seq[Literal]): Unit = {
      for (literal <- literals) {
        val indexByLiteral = unifiableUnitsIds.getOrElseUpdate(literal, unifiableUnitsBuff.size)
        if (indexByLiteral == unifiableUnitsBuff.size) {
          unifiableUnitsBuff += mutable.Set.empty
        }
        val set = unifiableUnitsBuff(indexByLiteral)
        for (newLiteral <- newLiterals) {
          if (newLiteral.polarity != literal.polarity) {
            unifyWithRename(Seq(literal.unit), Seq(newLiteral.unit)) match {
              case Some(_) =>
                set += newLiteral
              case None =>
            }
          }
        }
      }
    }

    def addProvedLiterals(newProvedLiterals: Seq[Literal]) = {
      provedLiterals ++= newProvedLiterals
      updateUnifiableUnits(newProvedLiterals)
    }

    def addNode(clause: Clause, node: CRProofNode): Unit = {
      val reverseId = reverseImplication.getOrElseUpdate(clause, bufferNodes.size)
      if (reverseId == bufferNodes.size) {
        bufferNodes += ArrayBuffer.empty
      }
      bufferNodes(reverseId) += node
    }

    def resolveUnitPropagations(clause: Clause, result: mutable.Set[Literal]): Unit = {
      // TODO: Isn't it important which node we will choose?
      val clauseNode = rnd.shuffle(bufferNodes(reverseImplication(clause))).head

      // TODO: Think about to shuffle literals to avoid worst case in the bruteforce.
      val shuffledLiterals = clause.literals

      val unifyCandidates = shuffledLiterals.map(id => unifiableUnitsBuff(unifiableUnitsIds(id)).toSeq)
      for (conclusionId <- unifyCandidates.indices) {
        // TODO: Think about other cut offs.
        if (unifyCandidates.indices.forall(id => id == conclusionId || unifyCandidates(id).nonEmpty)) {

          val unifiers: Seq[Seq[Literal]] = unifyCandidates.take(conclusionId) ++ unifyCandidates.drop(conclusionId + 1)
          val literals: Seq[Literal] = shuffledLiterals.take(conclusionId) ++ shuffledLiterals.drop(conclusionId + 1)

          def newPropagation(chosenUnifiers: mutable.Seq[Literal],
                             subs: mutable.Seq[Substitution],
                             globalSubst: Substitution,
                             usedVars: mutable.Set[Var]): Unit = {
            val unifierNodes = chosenUnifiers.map(l => bufferNodes(reverseImplication(l.toClause)).head)
            val curSubst = renameVars(shuffledLiterals(conclusionId).unit, usedVars)
            val unitPropagationNode =
              UnitPropagationResolution(
                unifierNodes,
                clauseNode,
                shuffledLiterals(conclusionId),
                literals,
                subs,
                globalSubst
              )
            val newLiteral = unitPropagationNode.conclusion.literal
            if (!result.contains(newLiteral) && !provedLiterals.contains(newLiteral)) {
              addNode(newLiteral.toClause, unitPropagationNode)
              result += newLiteral
            }
          }

          /**
            * Recursively generates UnitPropagationResolution and verifies unification on each step.
            *
            * @param chosenUnifiers    chosen literals
            * @param subs              substitutions for literals to ensure that variables in different literals are different
            * @param literalsWithSubst literals with unique variables
            * @param globalSubst       same global MGU for all literals
            * @param cur               number of chosen literals
            */
          def go(chosenUnifiers: mutable.Seq[Literal],
                 unifierWithSub: Seq[E],
                 subs: mutable.Seq[Substitution],
                 literalsWithSubst: Seq[Literal],
                 globalSubst: Substitution,
                 usedVars: mutable.Set[Var],
                 cur: Int): Unit = {

            if (cur == unifiers.size) {
              newPropagation(chosenUnifiers, subs, globalSubst, usedVars)
              return
            }

            // NOTE: Looking at all possible unifications turns to large complexity of this part of resolving
            // TODO: Think about to check only random K unifiers
            val candidates = {
              rnd.shuffle(unifiers(cur)).take(MAX_CNT_CANDIDATES)
            }
            for (curUni <- candidates) {
              val substitution = renameVars(curUni.unit, usedVars)
              val newSubs = subs :+ substitution(globalSubst)
              val leftWithSubst = globalSubst(substitution(curUni.unit))
              val newUnifierWithSubst = unifierWithSub :+ leftWithSubst

              // NOTE: Very dangerous to call pure unify method
              val unificationSubst = unify(leftWithSubst, literalsWithSubst(cur).unit)
              unificationSubst match {
                case Some(uniSubst) =>
                  go(chosenUnifiers :+ curUni,
                    newUnifierWithSubst.map(unificationSubst.get(_)),
                    newSubs.map(_ (unificationSubst.get)),
                    literalsWithSubst.map(unificationSubst.get(_)),
                    globalSubst(unificationSubst.get),
                    usedVars ++ leftWithSubst.variables,
                    cur + 1
                  )
                case None =>
              }
            }
          }

          go(mutable.Seq.empty,
            Seq.empty,
            mutable.Seq.empty,
            literals,
            Substitution.empty,
            mutable.Set[Var](literals.map(_.unit.variables).reduce {
              _ ++ _
            }: _*),
            0)
        }
      }
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      logger.debug("RESET")
      cdclClauses ++= newClauses
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses
        .flatMap(_.literals)(collection.breakOut)
      decisions.clear()

      reverseImplication.clear()
      bufferNodes.clear()

      cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))
      cdclClauses.foreach(node => addNode(node.conclusion, node))

      unifiableUnitsIds.clear()
      unifiableUnitsBuff.clear()

      provedLiterals.clear()
      addProvedLiterals(cnf.clauses.filter(_.isUnit).map(_.literal))
      addProvedLiterals(cdclClauses.toSeq.map(_.conclusion).filter(_.isUnit).map(_.literal))
      logger.debug(s"provedLiterals.size = ${provedLiterals.size}")
    }

    def removeDecisionLiterals(decisionLiterals: mutable.HashSet[Literal]): Unit = {
      decisions --= decisionLiterals

      def isValidCheck(node: CRProofNode): Boolean = {
        memIsValid.get(node) match {
          case Some(isValid) =>
            isValid
          case None =>
            val isValid: Boolean = node match {
              case Decision(literal) =>
                decisions.contains(literal)
              case Axiom(_) =>
                true
              case ConflictDrivenClauseLearning(_) =>
                true
              case UnitPropagationResolution(left, right, _, _, _) =>
                left.forall(isValidCheck) && isValidCheck(right)
              case Conflict(left, right) =>
                isValidCheck(left) && isValidCheck(right)
            }
            memIsValid.put(node, isValid)
            isValid
        }
      }

      for (literal <- provedLiterals) {
        val reverseId = reverseImplication(literal.toClause)
        bufferNodes(reverseId) = bufferNodes(reverseId).filter { cRProofNode: CRProofNode =>
          cRProofNode match {
            case ConflictDrivenClauseLearning(conflict) => isValidCheck(conflict)
            case other => isValidCheck(other)
          }
        }
      }
      val nonValidLiterals: Seq[Literal] = provedLiterals.toSeq
        .filter(literal => bufferNodes(reverseImplication(literal.toClause)).isEmpty)
      provedLiterals --= nonValidLiterals
      unifiableUnitsBuff.foreach(_ --= nonValidLiterals)
    }

    def getAllConflictDecisions(node: CRProofNode, acc: mutable.Set[Literal]): Unit =
      if (!memGetConflictDecisions.contains(node)) {
        memGetConflictDecisions.add(node)
        node match {
          case Decision(literal) =>
            acc += literal
          case Conflict(left, right) =>
            getAllConflictDecisions(left, acc)
            getAllConflictDecisions(right, acc)
          case UnitPropagationResolution(left, right, _, _, _) =>
            left.foreach(getAllConflictDecisions(_, acc))
            getAllConflictDecisions(right, acc)
          case ConflictDrivenClauseLearning(_) =>
          case Axiom(_) =>
        }
      }

    def updateVSIDS(newLiterals: Seq[Literal]): Unit = {
      literals.foreach(lit => activity.update(lit, activity.getOrElseUpdate(lit, 0) * DECAY_FACTOR))
      newLiterals.foreach(lit => activity.update(lit, activity.getOrElseUpdate(lit, 0) + BUMP))
    }

    def addCDCLClauses(newClauses: Set[CRProofNode]): Unit = {
      cdclClauses ++= newClauses
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)

      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      updateVSIDS(newClauses.flatMap(_.conclusion.literals)(collection.breakOut))

      newClauses.foreach(node =>
        addNode(node.conclusion, node))

      addProvedLiterals(newClauses.toSeq.map(_.conclusion).filter(_.isUnit).map(_.literal))
    }

    def makeDecision(available: Seq[Literal]): Literal = {
      // TODO: bad constant usage
      rnd.shuffle(available.sortWith(activity(_) > activity(_)).take(10)).head
    }

    def getBucketByExpr(expr: E): String = expr match {
      // NOTE: Var should be before Sym !!!
      case Var(_) =>
        VARIABLE_NAME
      case Sym(name) =>
        name
      case AppRec(Sym(name), args) =>
        name
    }

    addProvedLiterals(cnf.clauses.filter(_.isUnit).map(_.literal))
    cnf.clauses.foreach(clause => addNode(clause, Axiom(clause)))
    updateVSIDS(literals.toSeq)

    var cntWithoutDecisions = 0

    while (true) {
      logger.debug(s"new iteration:  provedLiterals(${provedLiterals.size})")
      val propagatedLiterals = mutable.Set.empty[Literal]
      for (clause <- nonUnitClauses)
        if (!clause.literals.exists(provedLiterals.contains)) {
          resolveUnitPropagations(clause, propagatedLiterals)
        }
      logger.debug(s"propagated ${propagatedLiterals.size}")
      addProvedLiterals(propagatedLiterals.toSeq)

      // find clauses of kind `A & !B` where there is some unification for {A = B}
      val CDCLClauses = mutable.Set.empty[CRProofNode]

      val provedLiteralBuckets: mutable.Map[String, ListBuffer[Literal]] = mutable.Map.empty
      for (literal <- provedLiterals.toSeq) {
        val bucketName = getBucketByExpr(literal.unit)

        val candidateLiterals = {
          if (bucketName == VARIABLE_NAME) {
            provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal])
          } else {
            provedLiteralBuckets.getOrElse(bucketName, ListBuffer.empty[Literal]) ++
              provedLiteralBuckets.getOrElse(VARIABLE_NAME, ListBuffer.empty[Literal])
          }
        }

        // NOTE: debug only
//        for (otherLiteral <- candidateLiterals if (literal.negated != otherLiteral.negated) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined) {
//          println(s"conflict(${bufferNodes(reverseImplication(literal)).size}, ${bufferNodes(reverseImplication(otherLiteral)).size})")
//          println(s"conflict($literal, $otherLiteral)")
//        }

        for {
          otherLiteral <- candidateLiterals if (literal.polarity != otherLiteral.polarity) && unifyWithRename(Seq(literal.unit), Seq(otherLiteral.unit)).isDefined
          conflictNode <- bufferNodes(reverseImplication(literal))
          otherNode <- bufferNodes(reverseImplication(otherLiteral))
          conflict = Conflict(conflictNode, otherNode)
        } {
          val cdclNode = ConflictDrivenClauseLearning(conflict)
          val newClause = cdclNode.conclusion
          if (newClause == Clause.empty) return Unsatisfiable(Some(Proof(conflict)))
          CDCLClauses += cdclNode
        }
        provedLiteralBuckets.getOrElseUpdate(VARIABLE_NAME, ListBuffer.empty).append(literal)
        if (bucketName != VARIABLE_NAME) {
          provedLiteralBuckets.getOrElseUpdate(bucketName, ListBuffer.empty).append(literal)
        }
      }
      if (CDCLClauses.nonEmpty) {
        logger.debug(s"found ${CDCLClauses.size} conflicts")

        val acc: mutable.HashSet[Literal] = mutable.HashSet.empty
        memGetConflictDecisions.clear()
        CDCLClauses.foreach {
          case ConflictDrivenClauseLearning(cl) => getAllConflictDecisions(cl, acc)
        }

        memIsValid.clear()
        removeDecisionLiterals(acc)
        addCDCLClauses(CDCLClauses.toSet)
      } else if (propagatedLiterals.isEmpty ||
            (cntWithoutDecisions >= MAX_CNT_WITHOUT_DECISIONS) ||
            (provedLiterals.size > MAX_PROVED_LITERALS_SIZE)) {
        cntWithoutDecisions = 0
        val available = (literals -- provedLiterals -- provedLiterals.map(!_)).toSeq
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          logger.debug("NEW DECISION")
          val decisionLiteral = makeDecision(available)
          addNode(decisionLiteral.toClause, Decision(decisionLiteral))
          addProvedLiterals(Seq(decisionLiteral))
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiterals(mutable.HashSet(!decisionLiteral))
          }
        }
      } else if (cnf.clauses.forall(clause => clause.literals.exists(provedLiterals.contains))) {
        val literals = provedLiterals ++ decisions
        val trueLiterals = literals.filter(_.polarity).map(_.unit).toSet
        val falseLiterals = literals.filterNot(_.polarity).map(_.unit).map(x => Neg(x)).toSet
        return Satisfiable(Some(new Assignment(trueLiterals ++ falseLiterals)))
      } else {
        // TODO: think about that case...
        cntWithoutDecisions += 1
      }
    }

    Error // this line is unreachable.
  }

  // scalastyle:on
}
