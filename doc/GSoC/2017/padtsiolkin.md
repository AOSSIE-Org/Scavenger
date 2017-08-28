# What I've done in [Scavenger-EP](https://gitlab.com/aossie/Scavenger)
 * Fixed some performance bugs in the EP version of the prover
 * Implemented the VSIDS heuristic, the set-of-support heuristic, and some other heuristics used for SAT-solver
 * Supported TPTP style, prepared the solver to CASC competition
 * Generalized prover for FOF 
 * Supported equality reasoning
 * Implemented parallelization for EP version of the prover

# Link to the [latest tag](https://gitlab.com/vlad10795/Scavenger/tags/GSoC-2017)

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

# Technical details
## Details about ExpertProver
We use N actors, where every actor works on a specific list of predicates. 
When an actor derives new CDCL rule, it sends the clause to all other actors.
When an actor receives a new clause, it applies rule Expertise(predicates) so it now works on 
its own predicates and consider another predicates as decisions.
## Details about Equality reasoning
Implemented the naive version of the equality reasoning: we just add 3+N+K new axioms, where N 
is the number of unique predicate symbols and K is the number of unique function symbols.
### Axioms:
 * `A=B => B=A`
 * `A=A`
 * `A=B => B=C => A=C`
 * for every k-ary predicate symbol `p`: `A_1=B_1 => ... => A_k=B_k => p(A_1, A_2, ..., A_k) => p(B_1, B_2, ..., B_k)`
 * for every k-ary functional symbol `fun`: `A_1=B_1 => ... => A_k=B_k => fun(A_1, ..., A_k)=fun(B_1, ..., B_k)`

## Links
 * [VSIDS heuristic](https://arxiv.org/pdf/1506.08905.pdf)
 * [Set of support](http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture9.html)
 * [TPTP syntax](http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html#formula_role)
 * [CASC competition](http://www.cs.miami.edu/~tptp/CASC/)
 * [TPTP Clausifier](http://www.cs.miami.edu/home/geoff/Papers/Journal/1996_SM96_SACJ.pdf)
 * [Equality reasoning](https://en.wikipedia.org/wiki/First-order_logic#Equality_and_its_axioms)
 * [parallelization](http://profs.sci.univr.it/~bonacina/papers/LNCS-2017PTP.pdf)



