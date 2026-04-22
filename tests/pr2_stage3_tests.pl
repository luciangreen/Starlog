:- module(pr2_stage3_tests, [run_pr2_stage3_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage3_tests :-
    run_tests([pr2_stage3_pipeline]).

:- begin_tests(pr2_stage3_pipeline).

triangular(N, R) :-
    R is N*(N+1)//2.

linear_3n_plus_1(N, R) :-
    R is 3*N + 1.

fibonacci(1, 1).
fibonacci(2, 1).
fibonacci(N, R) :-
    N > 2,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, R1),
    fibonacci(N2, R2),
    R is R1 + R2.

test(stage3_predicates_exist) :-
    assertion(current_predicate(starlog:npl_extract_numeric_samples/4)),
    assertion(current_predicate(starlog:npl_detect_polynomial_degree/3)),
    assertion(current_predicate(starlog:npl_validate_polynomial_fit/4)),
    assertion(current_predicate(starlog:npl_rewrite_recurrence_to_closed_form/4)).

test(stage3_extract_numeric_samples_from_callable_goal) :-
    starlog:npl_extract_numeric_samples(triangular(N, R), N, 3, Samples),
    assertion(Samples == [1-1, 2-3, 3-6]),
    forall(member(X-Y, Samples), triangular(X, Y)),
    var(R).

test(stage3_rewrite_triangular_to_closed_form) :-
    starlog:npl_rewrite_recurrence_to_closed_form(triangular(N, _R), N, Expr, Result),
    assertion(Result == accepted),
    N = 6,
    Value is Expr,
    assertion(Value =:= 21).

test(stage3_rewrite_linear_to_closed_form) :-
    starlog:npl_rewrite_recurrence_to_closed_form(linear_3n_plus_1(N, _R), N, Expr, Result),
    assertion(Result == accepted),
    N = 8,
    Value is Expr,
    assertion(Value =:= 25).

test(stage3_reject_non_polynomial_recurrence) :-
    starlog:npl_rewrite_recurrence_to_closed_form(fibonacci(N, _R), N, _Expr, Result),
    assertion(Result == rejected_non_polynomial).

test(stage3_reject_impure_recurrence) :-
    Goal = (writeln(side_effect), true),
    starlog:npl_rewrite_recurrence_to_closed_form(Goal, n, _Expr, Result),
    assertion(Result == rejected_impure).

:- end_tests(pr2_stage3_pipeline).
