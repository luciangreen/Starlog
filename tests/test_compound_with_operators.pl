% test_compound_with_operators.pl
% Tests for compound terms containing Starlog operators (: & •)
% This addresses the issue where compound terms with operators were
% being passed to arithmetic is/2 instead of being recognized as Starlog

:- use_module('../starlog_in_prolog').

% Helper predicates for testing
:- dynamic user:wrap/2.
:- dynamic user:unwrap/2.

user:wrap(X, [X]).
user:unwrap([X], X).

% Test 1: List with colon operator
% Previously: ERROR: Type error: `character' expected, found `a:a' (a compound)
test_list_with_colon :-
    starlog_call(A is [a:a]),
    A = ["aa"],
    write('✓ List with colon operator test passed'), nl.

% Test 2: Function call with colon operator argument
% Previously: ERROR: Unknown procedure: wrap/2 (expansion wasn't happening)
test_function_with_colon :-
    starlog_call(A is wrap(a:a)),
    A = ["aa"],
    write('✓ Function call with colon operator test passed'), nl.

% Test 3: Unregistered predicate with colon operator
% Previously: ERROR: Arithmetic: `(:)/2' is not a function
test_unregistered_with_colon :-
    starlog_call(A is foo(1:1)),
    A = foo("11"),  % 1:1 expands to string concatenation "11", result is foo("11")
    write('✓ Unregistered predicate with colon operator test passed'), nl.

% Test 4: Nested compound terms with operators
test_nested_compound :-
    starlog_call(A is [a:b, c:d]),
    A = ["ab", "cd"],
    write('✓ Nested compound with operators test passed'), nl.

% Test 5: Registered builtin with operator in argument
test_registered_with_operator :-
    starlog_call(A is reverse([a:a, b:b])),
    A = ["bb", "aa"],
    write('✓ Registered builtin with operator in argument test passed'), nl.

% Test 6: Compound with list append operator
test_compound_with_append :-
    starlog_call(A is wrap([1] & [2])),
    A = [[1, 2]],
    write('✓ Compound with list append operator test passed'), nl.

% Test 7: Compound with atom concatenation operator
test_compound_with_atom_concat :-
    starlog_call(A is wrap(a • b)),
    A = [ab],
    write('✓ Compound with atom concatenation operator test passed'), nl.

% Run all tests
run_tests :-
    write('Running compound term with operators tests...'), nl, nl,
    catch(test_list_with_colon, E1, (write('✗ Test 1 failed: '), write(E1), nl)),
    catch(test_function_with_colon, E2, (write('✗ Test 2 failed: '), write(E2), nl)),
    catch(test_unregistered_with_colon, E3, (write('✗ Test 3 failed: '), write(E3), nl)),
    catch(test_nested_compound, E4, (write('✗ Test 4 failed: '), write(E4), nl)),
    catch(test_registered_with_operator, E5, (write('✗ Test 5 failed: '), write(E5), nl)),
    catch(test_compound_with_append, E6, (write('✗ Test 6 failed: '), write(E6), nl)),
    catch(test_compound_with_atom_concat, E7, (write('✗ Test 7 failed: '), write(E7), nl)),
    nl,
    write('Compound term tests complete!'), nl.

:- initialization(run_tests, main).
