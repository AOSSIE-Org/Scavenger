# Improving performance [Scavenger-EP](https://gitlab.com/aossie/Scavenger)

I've fixed some ideological bugs in the EP version of the prover. I've implemented VSIDS heuristic, set-of-support heuristic, improved already written implementation for resolving UnitPropagation rule, improved performance for Conflict Driven Clause Learning rule. Adopted prover semantics to TPTP style and CASC competition.

# Implementing new Expert-Prover
I've adopted EP version of the Scavenger to parallelization using akka-actors. 

## Technical details
We have N actors where each actor works only with predicates specified exactly for this actor. The core of the actors is almost the same as in Scavenger-EP, but when we resovle CDCL rules it sends this new clauses to the all other actors.

### List of Merge Requests:
[Internal TPTP Clausifier](https://gitlab.com/aossie/Scavenger/merge_requests/2)
[Develop](https://gitlab.com/aossie/Scavenger/merge_requests/4)
[Implement VSIDS](https://gitlab.com/aossie/Scavenger/merge_requests/5)
[CASC](https://gitlab.com/aossie/Scavenger/merge_requests/6)
[FOF part](https://gitlab.com/aossie/Scavenger/merge_requests/7)
[Minor fixes before CASC](https://gitlab.com/aossie/Scavenger/merge_requests/8)
[Fix bug in forAllOut](https://gitlab.com/aossie/Scavenger/merge_requests/10)
[rename all variables to unique X_<number>](https://gitlab.com/aossie/Scavenger/merge_requests/11)
[refactor EPCR](https://gitlab.com/aossie/Scavenger/merge_requests/9)
[ExpertProver](https://gitlab.com/aossie/Scavenger/merge_requests/12)

