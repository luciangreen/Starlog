:- module(pr2_stage6_tests, [run_pr2_stage6_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage6_tests :-
    run_tests([pr2_stage6_index_formula_discovery]).

:- begin_tests(pr2_stage6_index_formula_discovery).

test(stage6_predicates_exist) :-
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)),
    assertion(current_predicate(starlog:npl_collect_formula_samples/3)),
    assertion(current_predicate(starlog:npl_build_polynomial_system/4)),
    assertion(current_predicate(starlog:npl_gaussian_elimination/3)).

test(stage6_reconstruct_offset_stride_and_power_formulas) :-
    Goal = flow([x-[1-4,2-5,3-6], y-[1-5,2-6,3-7], z-[1-5,2-7,3-9], u-[1-2,2-6,3-12]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    assertion(member(x-(4+i-1), Relations)),
    assertion(member(y-(5+i-1), Relations)),
    assertion(member(z-(2*i+3), Relations)),
    assertion(member(u-(i^2+i), Relations)).

test(stage6_collect_samples_from_relations) :-
    Relations = [x-(4+i-1), z-(2*i+3), u-(i^2+i)],
    starlog:npl_collect_formula_samples(Relations, i, SamplesByRelation),
    assertion(member(x-[1-4,2-5,3-6|_], SamplesByRelation)),
    assertion(member(z-[1-5,2-7,3-9|_], SamplesByRelation)),
    assertion(member(u-[1-2,2-6,3-12|_], SamplesByRelation)).

test(stage6_polynomial_coefficients_for_indexed_variable_use_gaussian) :-
    Goal = flow([u-[1-2,2-6,3-12]]),
    starlog:npl_collect_formula_samples(Goal, i, SamplesByRelation),
    member(u-Samples, SamplesByRelation),
    starlog:npl_detect_polynomial_degree(Samples, Degree),
    assertion(Degree =:= 2),
    starlog:npl_build_polynomial_system(Samples, Degree, Matrix, Vector),
    starlog:npl_gaussian_elimination(Matrix, Vector, Coeffs),
    assertion(Coeffs == [0,1,1]).

test(stage6_mixed_i_plus_1_and_i_squared_pipeline) :-
    Goal = flow([w-[1-5,2-10,3-17,4-26]]),
    starlog:npl_trace_index_flow(Goal, map([]), FlowGraph),
    starlog:npl_identify_independent_indices(FlowGraph, IndependentVars),
    starlog:npl_reconstruct_index_relations(FlowGraph, IndependentVars, Relations),
    assertion(member(w-(i^2+(2*i+2)), Relations)),
    starlog:npl_emit_direct_indexed_rule(FlowGraph, IndependentVars, Relations, DirectRule),
    starlog:npl_validate_direct_rule(Goal, DirectRule, Result),
    assertion(Result == accepted).

:- end_tests(pr2_stage6_index_formula_discovery).
