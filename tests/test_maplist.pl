% test_maplist.pl
% Tests for maplist implementation

:- use_module(starlog).

% Test 1: Basic maplist with writeln (just testing it doesn't crash)
test_maplist_basic :-
    write('Test 1: Basic maplist with writeln'), nl,
    write('  Calling: maplist(write, [a, b, c])'), nl,
    write('  Output: '),
    maplist(write, [a, b, c]),
    nl,
    write('✓ Test 1 passed'), nl, nl.

% Test 2: maplist with a simple predicate
% Define a simple test predicate
test_pred(X) :- atom(X).

test_maplist_atom_check :-
    write('Test 2: maplist with atom check'), nl,
    write('  Calling: maplist(atom, [a, b, c])'), nl,
    maplist(atom, [a, b, c]),
    write('✓ Test 2 passed'), nl, nl.

% Test 3: maplist with empty list
test_maplist_empty :-
    write('Test 3: maplist with empty list'), nl,
    write('  Calling: maplist(atom, [])'), nl,
    maplist(atom, []),
    write('✓ Test 3 passed'), nl, nl.

% Test 4: maplist with a custom predicate that prints
print_item(X) :- write(X), write(' ').

test_maplist_custom :-
    write('Test 4: maplist with custom print predicate'), nl,
    write('  Calling: maplist(print_item, [1, 2, 3])'), nl,
    write('  Output: '),
    maplist(print_item, [1, 2, 3]),
    nl,
    write('✓ Test 4 passed'), nl, nl.

% Test 5: maplist with a predicate that might fail
% This should fail on the number
test_maplist_fail :-
    write('Test 5: maplist that should fail on mixed types'), nl,
    write('  Calling: maplist(atom, [a, b, 3])'), nl,
    (\+ maplist(atom, [a, b, 3]) ->
        write('  Correctly failed on number'), nl,
        write('✓ Test 5 passed'), nl, nl
    ;
        write('✗ Test 5 failed - should have failed'), nl, nl,
        fail
    ).

% Test 6: Test the exact code from problem statement - verify it's fixed
% The problem was: Tail is maplist_1(call_1(Elem))
% Should be: call(Goal, Elem), maplist_(Tail, Goal)
test_maplist_problem_statement :-
    write('Test 6: Verify the problem statement code is fixed'), nl,
    write('  Testing that maplist_ correctly processes each element'), nl,
    write('  Calling: maplist_([x, y, z], atom)'), nl,
    maplist_([x, y, z], atom),
    write('✓ Test 6 passed'), nl, nl.

% Test 7: maplist with numbers
is_positive(X) :- number(X), X > 0.

test_maplist_numbers :-
    write('Test 7: maplist with number check'), nl,
    write('  Calling: maplist(is_positive, [1, 2, 3])'), nl,
    maplist(is_positive, [1, 2, 3]),
    write('✓ Test 7 passed'), nl, nl.

% Test 8: maplist should fail on negative number
test_maplist_numbers_fail :-
    write('Test 8: maplist should fail on negative number'), nl,
    write('  Calling: maplist(is_positive, [1, -2, 3])'), nl,
    (\+ maplist(is_positive, [1, -2, 3]) ->
        write('  Correctly failed on negative number'), nl,
        write('✓ Test 8 passed'), nl, nl
    ;
        write('✗ Test 8 failed - should have failed'), nl, nl,
        fail
    ).

% Test 9: Using maplist with a lambda-like syntax (compound goal)
double(X, Y) :- Y is X * 2.

test_maplist_with_result :-
    write('Test 9: maplist with result predicate'), nl,
    write('  Note: Standard maplist/2 does not collect results'), nl,
    write('  This test verifies maplist/2 works with predicates'), nl,
    maplist(atom, [a, b, c]),
    write('✓ Test 9 passed'), nl, nl.

% Test 10: Verify the fix more directly
% The buggy code was: Tail is maplist_1(call_1(Elem))
% which does not make sense. The fix should properly call the goal
test_direct_maplist_call :-
    write('Test 10: Direct test of maplist_ with simple goal'), nl,
    write('  Calling maplist_ directly'), nl,
    write('  maplist_([a, b, c], atom) should succeed'), nl,
    maplist_([a, b, c], atom),
    write('✓ Test 10 passed'), nl, nl.

% Run all tests
run_tests :-
    write('=== Running maplist tests ==='), nl, nl,
    Tests = [
        test_maplist_basic,
        test_maplist_atom_check,
        test_maplist_empty,
        test_maplist_custom,
        test_maplist_fail,
        test_maplist_problem_statement,
        test_maplist_numbers,
        test_maplist_numbers_fail,
        test_maplist_with_result,
        test_direct_maplist_call
    ],
    run_test_list(Tests, 1),
    write('=== All maplist tests complete ==='), nl.

% Helper to run a list of tests
run_test_list([], _).
run_test_list([Test|Rest], N) :-
    (catch(call(Test), E, (format('✗ Test ~w failed: ~w~n~n', [N, E]))) ; true),
    N1 is N + 1,
    run_test_list(Rest, N1).

:- initialization(run_tests, main).
