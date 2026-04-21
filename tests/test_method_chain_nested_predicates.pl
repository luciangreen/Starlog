% tests/test_method_chain_nested_predicates.pl
% Focused tests for nested predicate calls in method chains.

:- use_module('../starlog').

assert_equal(Actual, Expected, Label) :-
    (Actual = Expected ->
        format('✓ ~w~n', [Label])
    ;
        format('✗ ~w~n  Expected: ~w~n  Actual:   ~w~n', [Label, Expected, Actual]),
        fail
    ).

test_nested_predicate_in_method_args :-
    starlog_call(Result is reverse([3,1,2]) >> append(sort([4,2]))),
    assert_equal(Result, [2,4,2,1,3], 'Nested predicate in method arguments').

test_nested_predicate_in_base_and_method_args :-
    starlog_call(Result is reverse(sort([3,1,2])) >> append(reverse([4,2]))),
    assert_equal(Result, [2,4,3,2,1], 'Nested predicates in base and method arguments').

run_tests :-
    writeln('Running nested predicate method-chain tests...'),
    test_nested_predicate_in_method_args,
    test_nested_predicate_in_base_and_method_args,
    writeln('All nested predicate method-chain tests passed.').

:- initialization(run_tests, main).
