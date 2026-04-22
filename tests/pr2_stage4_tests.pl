:- module(pr2_stage4_tests, [run_pr2_stage4_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage4_tests :-
    run_tests([pr2_stage4_indexed_tracing]).

:- begin_tests(pr2_stage4_indexed_tracing).

test(stage4_predicates_exist) :-
    assertion(current_predicate(starlog:npl_assign_symbolic_indices/3)),
    assertion(current_predicate(starlog:npl_reduce_predicate_to_pattern_irreducibles/2)),
    assertion(current_predicate(starlog:npl_trace_index_flow/3)),
    assertion(current_predicate(starlog:npl_identify_independent_indices/2)),
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)),
    assertion(current_predicate(starlog:npl_reconstruct_direct_indexed_rule/3)),
    assertion(current_predicate(starlog:npl_validate_direct_rule/3)).

test(stage4_assign_symbolic_indices_exposes_inspectable_map) :-
    Structure = [x(4), x(5), x(6)],
    starlog:npl_assign_symbolic_indices(Structure, indexed(Structure), map(Map)),
    assertion(Map \== []),
    assertion(member(path([1])-x(4), Map)),
    assertion(member(path([2])-x(5), Map)),
    assertion(member(path([3])-x(6), Map)).

test(stage4_reduce_to_pattern_and_irreducibles) :-
    Goal = (member(X, [a,b,c]), nth1(I, [a,b,c], X)),
    starlog:npl_reduce_predicate_to_pattern_irreducibles(Goal, Reduced),
    assertion(Reduced = reduced(_)).

test(stage4_trace_and_identify_independent_index) :-
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    assertion(IndependentVars == [i]).

test(stage4_reconstruct_index_relations_from_first_principles) :-
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7], u-[1-2,2-6,3-12]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    assertion(member(x-(4+i-1), Relations)),
    assertion(member(y-(5+i-1), Relations)),
    assertion(member(u-(i^2+i), Relations)).

test(stage4_reconstruct_and_validate_direct_rule) :-
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    starlog:npl_emit_direct_indexed_rule(FlowGraph, IndependentVars, Relations, DirectRule),
    starlog:npl_validate_direct_rule(Goal, DirectRule, Result),
    assertion(Result == accepted).

test(stage4_negative_non_reducible_predicate) :-
    Goal = non_reducible_external_call(some_foreign_system),
    starlog:npl_reduce_predicate_to_pattern_irreducibles(Goal, Reduced),
    assertion(Reduced == non_reducible).

test(stage4_negative_ambiguous_mapping_rejected) :-
    AmbiguousFlow = flow([x-[1-4,2-5], x-[1-4,2-7]]),
    starlog:npl_detect_ambiguous_index_mapping(AmbiguousFlow, Ambiguous),
    assertion(Ambiguous == true).

:- end_tests(pr2_stage4_indexed_tracing).
