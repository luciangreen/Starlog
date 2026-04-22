:- module(pr2_stage1_failing_tests, [run_pr2_stage1_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage1_tests :-
    run_tests([pr2_stage1a_gaussian, pr2_stage1b_indexed_optimisation]).

:- begin_tests(pr2_stage1a_gaussian).

% Stage 1A / A1: triangular numbers polynomial discovery via Gaussian elimination.
test(a1_triangular_requires_gaussian_pipeline) :-
    assertion(current_predicate(starlog:npl_detect_polynomial_degree/2)),
    assertion(current_predicate(starlog:npl_build_polynomial_system/4)),
    assertion(current_predicate(starlog:npl_gaussian_elimination/3)),
    assertion(current_predicate(starlog:npl_reconstruct_polynomial/3)),
    Samples = [1-1, 2-3, 3-6],
    starlog:npl_detect_polynomial_degree(Samples, Degree),
    assertion(Degree =:= 2),
    starlog:npl_build_polynomial_system(Samples, Degree, Matrix, Vector),
    starlog:npl_gaussian_elimination(Matrix, Vector, Coefficients),
    starlog:npl_reconstruct_polynomial(n, Coefficients, Expr),
    assertion(nonvar(Expr)).

% Stage 1A / A2: linear sequence reconstruction.
test(a2_linear_sequence_3n_plus_1) :-
    assertion(current_predicate(starlog:npl_build_polynomial_system/4)),
    assertion(current_predicate(starlog:npl_gaussian_elimination/3)),
    Samples = [1-4, 2-7, 3-10],
    Degree = 1,
    starlog:npl_build_polynomial_system(Samples, Degree, Matrix, Vector),
    starlog:npl_gaussian_elimination(Matrix, Vector, Coefficients),
    assertion(Coefficients == [1,3] ; Coefficients == [3,1]).

% Stage 1A / A3: cubic sequence reconstruction.
test(a3_cubic_sequence_degree3) :-
    assertion(current_predicate(starlog:npl_detect_polynomial_degree/2)),
    Samples = [1-1, 2-8, 3-27, 4-64],
    starlog:npl_detect_polynomial_degree(Samples, Degree),
    assertion(Degree =:= 3).

% Stage 1A / A4: non-polynomial recurrence should not be rewritten.
test(a4_fibonacci_rejected) :-
    assertion(current_predicate(starlog:npl_validate_polynomial_formula/4)),
    FibSamples = [1-1, 2-1, 3-2, 4-3, 5-5, 6-8],
    starlog:npl_validate_polynomial_formula(FibSamples, _, n, Result),
    assertion(Result == rejected_non_polynomial).

% Stage 1A / A5: impure recurrence should not be rewritten.
test(a5_impure_predicate_rejected) :-
    assertion(current_predicate(starlog:npl_rewrite_recurrence_to_closed_form/4)),
    Goal = (writeln(side_effect), true),
    starlog:npl_rewrite_recurrence_to_closed_form(Goal, n, _Expr, Result),
    assertion(Result == rejected_impure).

:- end_tests(pr2_stage1a_gaussian).

:- begin_tests(pr2_stage1b_indexed_optimisation).

% Stage 1B / B1: derive direct indexed output from traced correspondences.
test(b1_indexed_structure_generation_and_selected_output_formula) :-
    assertion(current_predicate(starlog:npl_assign_symbolic_indices/3)),
    assertion(current_predicate(starlog:npl_trace_index_flow/3)),
    assertion(current_predicate(starlog:npl_emit_direct_indexed_rule/4)).

% Stage 1B / B2: reconstruct indexed-variable sequences x_i=4+i-1, y_i=5+i-1.
test(b2_indexed_variable_sequence_reconstruction) :-
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)),
    FlowGraph = flow([x- [1-4,2-5,3-6], y-[1-5,2-6,3-7]]),
    starlog:npl_reconstruct_index_relations(FlowGraph, [i], Relations),
    assertion(member(x-(4+i-1), Relations)),
    assertion(member(y-(5+i-1), Relations)).

% Stage 1B / B3: multiplication-derived selected pattern.
test(b3_multiplication_derived_selected_pattern) :-
    assertion(current_predicate(starlog:npl_trace_index_flow/3)),
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)).

% Stage 1B / B4: addition-derived selected pattern.
test(b4_addition_derived_selected_pattern) :-
    assertion(current_predicate(starlog:npl_trace_index_flow/3)),
    assertion(current_predicate(starlog:npl_reconstruct_index_relations/3)).

% Stage 1B / B5: non-special-case patterned extraction.
test(b5_non_special_case_patterned_extraction) :-
    assertion(current_predicate(starlog:npl_reduce_predicate_to_pattern_irreducibles/2)),
    assertion(current_predicate(starlog:npl_emit_direct_indexed_rule/4)).

% Stage 1B / B6: polynomial indexed-variable derivation via Gaussian elimination.
test(b6_polynomial_indexed_variable_derivation) :-
    assertion(current_predicate(starlog:npl_collect_formula_samples/3)),
    assertion(current_predicate(starlog:npl_gaussian_elimination/3)).

% Stage 1B / B7: negative test when full structure is required.
test(b7_negative_full_structure_required) :-
    assertion(current_predicate(starlog:npl_should_preserve_full_structure/2)).

% Stage 1B / B8: negative test for ambiguous mapping.
test(b8_negative_ambiguous_mapping) :-
    assertion(current_predicate(starlog:npl_detect_ambiguous_index_mapping/2)).

% Stage 1B / B9: negative test for non-reducible predicate.
test(b9_negative_non_reducible_predicate) :-
    assertion(current_predicate(starlog:npl_reduce_predicate_to_pattern_irreducibles/2)),
    Goal = non_reducible_external_call(_),
    starlog:npl_reduce_predicate_to_pattern_irreducibles(Goal, Result),
    assertion(Result == non_reducible).

:- end_tests(pr2_stage1b_indexed_optimisation).
