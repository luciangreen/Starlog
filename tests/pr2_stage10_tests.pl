:- module(pr2_stage10_tests, [run_pr2_stage10_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage10_tests :-
    run_tests([pr2_stage10_starlog_integration]).

:- begin_tests(pr2_stage10_starlog_integration).

test(stage10_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage10_integration_option/1)),
    assertion(current_predicate(starlog:npl_stage10_gaussian_canonical_repository/1)),
    assertion(current_predicate(starlog:npl_stage10_gaussian_alignment/1)).

test(stage10_documents_port_and_align_decision) :-
    starlog:npl_stage10_integration_option(Option),
    assertion(Option == option_b_port_and_align).

test(stage10_documents_starlog_as_canonical_gaussian_repository) :-
    starlog:npl_stage10_gaussian_canonical_repository(CanonicalRepo),
    assertion(CanonicalRepo == starlog).

test(stage10_alignment_maps_neuroprolog_surface_to_starlog_gaussian_core) :-
    starlog:npl_stage10_gaussian_alignment(Alignment),
    assertion(member(canonical_module(gaussian_elimination), Alignment)),
    assertion(member(npl_gaussian_elimination_3_uses(gaussian_elimination_3), Alignment)),
    assertion(member(npl_solve_polynomial_coeffs_3_uses(gaussian_elimination_3), Alignment)),
    assertion(member(stage9_codegen_uses(npl_gaussian_elimination_3), Alignment)).

test(stage10_alignment_is_deterministic) :-
    starlog:npl_stage10_gaussian_alignment(A),
    starlog:npl_stage10_gaussian_alignment(B),
    assertion(A == B).

:- end_tests(pr2_stage10_starlog_integration).
