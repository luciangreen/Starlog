:- module(pr2_stage5_tests, [run_pr2_stage5_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage5_tests :-
    run_tests([pr2_stage5_general_index_tracing]).

:- begin_tests(pr2_stage5_general_index_tracing).

test(stage5_no_named_special_case_optimiser_predicates) :-
    assertion(\+ current_predicate(starlog:npl_optimize_diagonal/4)),
    assertion(\+ current_predicate(starlog:npl_optimize_row/4)),
    assertion(\+ current_predicate(starlog:npl_optimize_column/4)),
    assertion(\+ current_predicate(starlog:npl_optimize_anti_diagonal/4)).

test(stage5_named_case_goal_is_handled_as_general_reducible_custom) :-
    Goal = diagonal(I, I, _V),
    starlog:npl_reduce_predicate_to_pattern_irreducibles(Goal, Reduced),
    assertion(Reduced = reduced(node(reducible_custom, diagonal/3, Goal))).

test(stage5_trace_reports_general_first_principles_strategy) :-
    Goal = flow([x-[1-4,2-5,3-6]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    assertion(FlowGraph = flow_graph(_, _, _, trace(Meta))),
    assertion(member(optimisation_class(general_first_principles), Meta)),
    assertion(member(named_special_cases(none), Meta)).

test(stage5_index_formula_still_derived_from_general_pipeline) :-
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    assertion(member(x-(4+i-1), Relations)),
    assertion(member(y-(5+i-1), Relations)).

:- end_tests(pr2_stage5_general_index_tracing).
