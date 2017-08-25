# What I've done in [Scavenger-EP](https://gitlab.com/aossie/Scavenger)

 * Fixed some ideological bugs in the EP version of the prover
 * Implemented VSIDS heuristic, set-of-support heuristic, and some other heuristics used for SAT-solver
 * Supported TPTP style, prepared to CASC competition
 * Generalized prover for FOF 
 * Supported equality reasoning
 * Implemented parallelization for EP version of the prover

# Technical details
## Links
### [VSIDS heuristic](https://arxiv.org/pdf/1506.08905.pdf)
### [Set of support](http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture9.html)
### [TPTP syntax](http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html#formula_role)
### [CASC competition](http://www.cs.miami.edu/~tptp/CASC/)
### [TPTP Clausifier](http://www.cs.miami.edu/home/geoff/Papers/Journal/1996_SM96_SACJ.pdf)
### [Equality reasoning](https://en.wikipedia.org/wiki/First-order_logic#Equality_and_its_axioms)
### [parallelization](http://profs.sci.univr.it/~bonacina/papers/LNCS-2017PTP.pdf)
## Technical details about ExpertProver
We use N actors where every actor works on a specific list of predicates. 
When actor derives new CDCL rule it sends the clause to all other actors.
When actor receives new clause it applies rule Expertise(predicates) so it now works on 
it's own predicates and consider another predicats as decisions.

# List of Merge Requests:
 * [Internal TPTP Clausifier](https://gitlab.com/aossie/Scavenger/merge_requests/2)
 * [Develop](https://gitlab.com/aossie/Scavenger/merge_requests/4)
 * [Implement VSIDS](https://gitlab.com/aossie/Scavenger/merge_requests/5)
 * [CASC](https://gitlab.com/aossie/Scavenger/merge_requests/6)
 * [FOF part](https://gitlab.com/aossie/Scavenger/merge_requests/7)
 * [Minor fixes before CASC](https://gitlab.com/aossie/Scavenger/merge_requests/8)
 * [Fix bug in forAllOut](https://gitlab.com/aossie/Scavenger/merge_requests/10)
 * [rename all variables to unique X_<number>](https://gitlab.com/aossie/Scavenger/merge_requests/11)
 * [refactor EPCR](https://gitlab.com/aossie/Scavenger/merge_requests/9)
 * [ExpertProver](https://gitlab.com/aossie/Scavenger/merge_requests/12)

