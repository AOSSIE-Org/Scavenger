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
class EPCR(maxCountCandidates: Int = 1000,
           maxCountWithoutDecisions: Int = 10,
           maxProvedLiteralsSize: Int = 10000,
           initialBump: Double = 1.0,
           decayFactor: Double = 0.99,
           maxActivity: Double = 1e10,
           randomDecisionsPercent: Double = 5) extends Prover {
  //   TODO: Do research about these constants

  //   TODO: Think about every usage of randomness
  val rnd = new Random(107)

  // FIXME: Bad practice to use predefined name(could be collision)
  val VARIABLE_NAME: String = "___VARIABLE___"

  /**
    * Minisat implementation
    */
  var incSym: Double = initialBump

  // scalastyle:off
  override def prove(cnf: CNF): ProblemStatus = {
    val logger = Logger(LoggerFactory.getLogger("prover"))

    if (cnf.clauses.contains(Clause.empty)) {
      return Unsatisfiable(Some(Proof(Axiom(Clause.empty))))
    }

    val initialClauses = cnf.clauses.to[ListBuffer]

    val predicates = cnf.predicates
    val isEqualityReasoning = predicates.contains((new Sym("=") with Infix, 2))
    if (isEqualityReasoning) {
      logger.info("Equality reasoning problem")
      /**
        * symmetry axiom
        */
      initialClauses.append(
        Clause(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("B"))))
        (AppRec(new Sym("=") with Infix, Seq(Var("B"), Var("A")))))
      /**
        * reflexivity axiom
        */
      initialClauses.append(Clause()(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("A")))))
      /**
        * transitivity axiom
        */
      initialClauses.append(
        Clause(AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("B"))), AppRec(new Sym("=") with Infix, Seq(Var("B"), Var("C"))))
        (AppRec(new Sym("=") with Infix, Seq(Var("A"), Var("C")))))
      /** congruence axioms for predicates
        */
      predicates.foreach { case (predicate, arity) =>
        // TODO: add support for airty > 13
        if (2 * arity <= 26) {
          val leftVariables =
            List.range('A'.toInt, 'A'.toInt + arity).map(ind => Var(ind.toChar.toString))
          val rightVariables =
            List.range('A'.toInt + arity, 'A'.toInt + 2 * arity).map(ind => Var(ind.toChar.toString))
          val equalities = leftVariables.zip(rightVariables).map {
            case (left, right) => AppRec(new Sym("=") with Infix, Seq(left, right))
          }
          val predicateOnLeft = AppRec(predicate, leftVariables)
          val predicateOnRight = AppRec(predicate, rightVariables)
          val congAxiom = Clause(equalities :+ predicateOnLeft: _*)(predicateOnRight)
          initialClauses.append(congAxiom)
        } else {
          logger.warn(s"predicates with arity(=$arity) > 13 not supported")
        }
      }
      /** congruence axioms for functions
        */
      val funs = cnf.functionSymbols.filter(_._2 > 0)
      funs.foreach { case (fun, arity) =>
        // TODO: for arity > 13
        if (2 * arity <= 26) {
          val leftVariables =
            List.range('A'.toInt, 'A'.toInt + arity).map(ind => Var(ind.toChar.toString))
          val rightVariables =
            List.range('A'.toInt + arity, 'A'.toInt + 2 * arity).map(ind => Var(ind.toChar.toString))
          val equalities = leftVariables.zip(rightVariables).map {
            case (left, right) => AppRec(new Sym("=") with Infix, Seq(left, right))
          }
          val leftFun = AppRec(fun, leftVariables)
          val rightFun = AppRec(fun, rightVariables)
          val predicateOnRight = AppRec(Sym("="), Seq(leftFun, rightFun))
          val congAxiom = Clause(equalities: _*)(predicateOnRight)
          initialClauses.append(congAxiom)
        } else {
          logger.warn(s"functions with arity(=$arity) > 13 not supported")
        }
      }
    }

    def to3CNF(clauses: ListBuffer[Clause]): Unit = {

      def genNewSym(usedSyms: mutable.HashSet[Sym]): Sym = {
        val c = (rnd.nextInt(26) + 'a'.toInt).toChar
        var i = 0
        while (usedSyms.contains(Sym(s"$c$i"))) i += 1
        val newSym = Sym(s"$c$i")
        usedSyms.add(newSym)
        newSym
      }

      def literalsToClause(literals: Literal*): Clause = {
        val (positiveLiterals, negativeLiterals) = literals.partition(_.polarity)
        Clause(negativeLiterals.map(_.unit): _*)(positiveLiterals.map(_.unit): _*)
      }

      val usedSyms = clauses.flatMap(clause =>
        clause.literals.flatMap(_.unit.functionSymbols.map(_._1))
      )(collection.breakOut).to[mutable.HashSet]
      val nClauses = ListBuffer.empty[Clause]
      for (clause <- clauses) {
        var lits = clause.literals.to[ArrayBuffer]
        while (lits.size > 3) {
          val nlits = ArrayBuffer.empty[Literal]
          for (idx <- 0 until lits.length / 2) {
            val tmpSym = genNewSym(usedSyms)
            val args = (lits(idx * 2).unit.variables ++ lits(idx * 2 + 1).unit.variables).distinct
            val fun = Literal(AppRec(tmpSym, args), true)
            nClauses += literalsToClause(!fun, lits(idx * 2), lits(idx * 2 + 1))
            nClauses += literalsToClause(!lits(idx * 2), fun)
            nClauses += literalsToClause(!lits(idx * 2 + 1), fun)
            nlits += fun
          }
          if (lits.length % 2 == 1) nlits += lits.last
          lits = nlits
        }
        nClauses += literalsToClause(lits: _*)
      }
      clauses.clear
      clauses ++= nClauses
    }

//    to3CNF(initialClauses)

    /**
      * Mutable set of proved literals initialized with the input CNF's unit clauses.
      */
    val provedLiterals: mutable.Set[Literal] = mutable.Set.empty

    /**
      * Non-unit clauses from the input CNF plus CDCL clauses.
      */
    var nonUnitClauses: mutable.Set[Clause] = mutable.Set(initialClauses.filter(!_.isUnit): _*)

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
    val cdclClauses: mutable.Map[Clause, CRProofNode] = mutable.Map.empty

    /**
      * VSIDS haracteristic helps to choose decision.
      */
    val activity: mutable.Map[Sym, Double] = mutable.HashMap.empty

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
      for (clauseNode <- bufferNodes(reverseImplication(clause))) {

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
                rnd.shuffle(unifiers(cur)).take(maxCountCandidates)
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
    }

    def reset(newClauses: Set[CRProofNode]): Unit = {
      logger.debug("RESET")
      cdclClauses ++= newClauses.map(node => (node.conclusion, node))
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)
      literals = nonUnitClauses
        .flatMap(_.literals)(collection.breakOut)
      decisions.clear()

      reverseImplication.clear()
      bufferNodes.clear()

      initialClauses.foreach(clause => addNode(clause, Axiom(clause)))
      cdclClauses.foreach(clauseNode => addNode(clauseNode._1, clauseNode._2))

      unifiableUnitsIds.clear()
      unifiableUnitsBuff.clear()

      provedLiterals.clear()
      addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
      addProvedLiterals(cdclClauses.toSeq.map(_._1).filter(_.isUnit).map(_.literal))
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

    def bumpActivity(literal: Literal): Unit = {
      val syms = literal.predicates.map(_._1)
      var overhead: Boolean = false
      syms.foreach { sym =>
        activity.update(sym, activity.getOrElseUpdate(sym, 0) + incSym)
        overhead |= (activity(sym) >= maxActivity)
      }
      if (overhead) {
        activity.transform {
          case (key, value) => value / maxActivity
        }
        incSym /= maxActivity
      }
    }

    def updateInc(): Unit = incSym /= decayFactor

    def addCDCLClauses(nodes: Set[CRProofNode]): Unit = {
      val newClauses = nodes.filterNot(node => cdclClauses.contains(node.conclusion))
      cdclClauses ++= newClauses.map(node => (node.conclusion, node))
      nonUnitClauses ++= newClauses.map(_.conclusion).filter(!_.isUnit)

      literals = nonUnitClauses.flatMap(_.literals)(collection.breakOut)
      newClauses.toSeq.flatMap(_.conclusion.literals)(collection.breakOut).foreach(bumpActivity)
      updateInc()

      newClauses.foreach(node =>
        addNode(node.conclusion, node))

      addProvedLiterals(newClauses.toSeq.map(_.conclusion).filter(_.isUnit).map(_.literal))

      logger.debug(s"added ${newClauses.size} new conflicts")
    }

    def getActivity(literal: Literal): Double = {
      val funs = literal.predicates
      funs.map {
        case (sym, arity) => activity.getOrElse(sym, 0.0)
      }.sum * 1.0 / funs.size
    }

    /**
      * Implementation of miniSAT version of VSIDS heuristic
      *
      * @param available set of candidates for choosing as a new decision
      * @return new decision
      */
    def makeDecision(available: Seq[Literal]): Literal = {
      val res = {
        if (rnd.nextInt(101) >= randomDecisionsPercent) {
          val availableActivities = available.map(getActivity)
          val availableActivitiesSum = availableActivities.sum
//          val alpha = (math.log(incSym) - math.log(available.size)) * availableActivitiesSum / availableActivities.max
          val probs = availableActivities.map(act => math.exp(act * 20 / availableActivitiesSum))
          val p = rnd.nextDouble * probs.sum
          var curP = 0.0
          for ((literal, prob) <- available.zip(probs)) {
            curP += prob
            if (p <= curP) {
              return literal
            }
          }
        }
        available(rnd.nextInt(available.size))
      }
      res
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

    addProvedLiterals(initialClauses.filter(_.isUnit).map(_.literal))
    initialClauses.foreach(clause => addNode(clause, Axiom(clause)))

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
        val acc: mutable.HashSet[Literal] = mutable.HashSet.empty
        memGetConflictDecisions.clear()
        CDCLClauses.foreach {
          case ConflictDrivenClauseLearning(cl) =>
            getAllConflictDecisions(cl, acc)
        }

        memIsValid.clear()
        removeDecisionLiterals(acc)
        addCDCLClauses(CDCLClauses.toSet)
      } else if (propagatedLiterals.isEmpty ||
        (cntWithoutDecisions >= maxCountWithoutDecisions) ||
        (provedLiterals.size > maxProvedLiteralsSize)) {
        cntWithoutDecisions = 0
        val available = (literals -- provedLiterals -- provedLiterals.map(!_)).toSeq
        if (available.isEmpty) {
          reset(Set.empty)
        } else {
          val decisionLiteral = makeDecision(available)
          addNode(decisionLiteral.toClause, Decision(decisionLiteral))
          addProvedLiterals(Seq(decisionLiteral))
//          val decisionActivity = getActivity(decisionLiteral)
//          println(s"NEW DECISION: $decisionLiteral, activity: $decisionActivity")
          decisions += decisionLiteral
          if (decisions.contains(!decisionLiteral)) {
            removeDecisionLiterals(mutable.HashSet(!decisionLiteral))
          }
        }
      } else if (initialClauses.forall(clause => clause.literals.exists(provedLiterals.contains))) {
        val literals = provedLiterals ++ decisions
        val (positiveLiterals, negativeLiterals) = literals.partition(_.polarity)
        return Satisfiable(Some(new Assignment(positiveLiterals.map(_.unit).toSet ++ negativeLiterals.map(_.unit).toSet)))
      } else {
        // TODO: think about that case...
        cntWithoutDecisions += 1
      }
    }

    Error // this line is unreachable.
  }

  // scalastyle:on
}

object EPCR extends EPCR(
  maxCountCandidates = 1000,
  maxCountWithoutDecisions = 5,
  maxProvedLiteralsSize = 10000,
  initialBump = 1.0,
  decayFactor = 0.99,
  maxActivity = 1e10,
  randomDecisionsPercent = 5)