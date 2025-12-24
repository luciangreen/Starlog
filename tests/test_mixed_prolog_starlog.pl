% test_mixed_prolog_starlog.pl
% Tests for mixing Prolog and Starlog code in the same file

:- use_module('../starlog_in_prolog').

% Standard Prolog predicate
is_even(N) :- 0 is N mod 2.

% Starlog-style predicate (using term expansion)
concat_strings(A, B, C) :- C is A:B.

% Mixed style predicate
process_list(List, Result) :-
    length(List, Len),
    Result is reverse(List).

% Test standard Prolog works
test_standard_prolog :-
    is_even(4),
    \+ is_even(3),
    write('✓ Standard Prolog test passed'), nl.

% Test Starlog expansion in clause
test_starlog_in_clause :-
    concat_strings("hello", "world", Result),
    Result = "helloworld",
    write('✓ Starlog in clause test passed'), nl.

% Test mixed code
test_mixed :-
    process_list([1,2,3], R),
    R = [3,2,1],
    write('✓ Mixed Prolog/Starlog test passed'), nl.

% Test that standard Prolog predicates work normally
test_standard_predicates :-
    append([1,2], [3,4], R1),
    R1 = [1,2,3,4],
    member(2, [1,2,3]),
    length([a,b,c], 3),
    write('✓ Standard predicates test passed'), nl.

% Test Starlog call with standard Prolog
test_starlog_call_mixed :-
    starlog_call((A is [1,2] & [3,4])),
    length(A, L),
    L = 4,
    write('✓ Starlog call mixed test passed'), nl.

% Run all tests
run_tests :-
    write('Running mixed Prolog/Starlog tests...'), nl, nl,
    catch(test_standard_prolog, E, (write('✗ Standard Prolog failed: '), write(E), nl)),
    catch(test_starlog_in_clause, E2, (write('✗ Starlog in clause failed: '), write(E2), nl)),
    catch(test_mixed, E3, (write('✗ Mixed failed: '), write(E3), nl)),
    catch(test_standard_predicates, E4, (write('✗ Standard predicates failed: '), write(E4), nl)),
    catch(test_starlog_call_mixed, E5, (write('✗ Starlog call mixed failed: '), write(E5), nl)),
    nl,
    write('Mixed Prolog/Starlog tests complete!'), nl.

:- initialization(run_tests, main).
