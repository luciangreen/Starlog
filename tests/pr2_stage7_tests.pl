:- module(pr2_stage7_tests, [run_pr2_stage7_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage7_tests :-
    run_tests([pr2_stage7_required_predicates]).

:- begin_tests(pr2_stage7_required_predicates).

test(stage7_predicates_exist) :-
    assertion(current_predicate(starlog:npl_assign_symbolic_indices/3)),
    assertion(current_predicate(starlog:npl_reduce_predicate_to_pattern_irreducibles/2)),
    assertion(current_predicate(starlog:npl_trace_index_flow/3)),
    assertion(current_predicate(starlog:npl_identify_independent_indices/2)),
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)),
    assertion(current_predicate(starlog:npl_collect_formula_samples/3)),
    assertion(current_predicate(starlog:npl_build_polynomial_system/4)),
    assertion(current_predicate(starlog:npl_gaussian_elimination/3)),
    assertion(current_predicate(starlog:npl_reconstruct_direct_indexed_rule/3)),
    assertion(current_predicate(starlog:npl_validate_direct_rule/3)).

test(stage7_general_indexed_tracing_pipeline_end_to_end) :-
    Structure = [x(4), x(5), x(6)],
    starlog:npl_assign_symbolic_indices(Structure, indexed(Structure), map(Map)),
    assertion(Map \== []),
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7], u-[1-2,2-6,3-12]]),
    starlog:npl_reduce_predicate_to_pattern_irreducibles((member(V, [4,5,6]), V > 0), Reduced),
    assertion(Reduced = reduced(_)),
    starlog:npl_trace_index_flow(Goal, map(Map), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    assertion(IndependentVars == [i]),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    assertion(member(x-(4+i-1), Relations)),
    assertion(member(y-(5+i-1), Relations)),
    assertion(member(u-(i^2+i), Relations)),
    starlog:npl_collect_formula_samples(Relations, i, SamplesByRelation),
    member(u-PolynomialSamples, SamplesByRelation),
    starlog:npl_detect_polynomial_degree(PolynomialSamples, Degree),
    assertion(Degree =:= 2),
    starlog:npl_build_polynomial_system(PolynomialSamples, Degree, Matrix, Vector),
    starlog:npl_gaussian_elimination(Matrix, Vector, Coeffs),
    assertion(Coeffs == [0,1,1]),
    starlog:npl_reconstruct_direct_indexed_rule(Relations, Coeffs, DirectRule),
    assertion(DirectRule = direct_index_rule(Relations, coefficient_metadata([0,1,1]))),
    starlog:npl_validate_direct_rule(Goal, DirectRule, Result),
    assertion(Result == accepted).

:- end_tests(pr2_stage7_required_predicates).
