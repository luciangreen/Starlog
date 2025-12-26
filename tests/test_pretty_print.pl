% test_pretty_print.pl
% Tests for pretty printing functionality

:- use_module('../starlog_in_prolog').

% Test 1: Simple conjunction
test_conjunction :-
    write('Test 1: Conjunction (and)'), nl,
    write('Input: string_concat("a","b",T1), string_concat(T1,"c",T2)'), nl,
    write('Output:'), nl,
    starlog_output_code((string_concat("a","b",T1), string_concat(T1,"c",T2))),
    nl.

% Test 2: Disjunction
test_disjunction :-
    write('Test 2: Disjunction (or)'), nl,
    write('Input: (A = 1 ; A = 2)'), nl,
    write('Output:'), nl,
    starlog_output_code((A = 1 ; A = 2)),
    nl.

% Test 3: Negation with simple goal
test_negation_simple :-
    write('Test 3: Negation with simple goal'), nl,
    write('Input: \\+ member(A, [1,2,3])'), nl,
    write('Output:'), nl,
    starlog_output_code(\+ member(A, [1,2,3])),
    nl.

% Test 4: If-then-else
test_if_then_else :-
    write('Test 4: If-then-else'), nl,
    write('Input: (A > 5 -> B = big ; B = small)'), nl,
    write('Output:'), nl,
    starlog_output_code((A > 5 -> B = big ; B = small)),
    nl.

% Test 5: If-then (no else)
test_if_then :-
    write('Test 5: If-then (no else)'), nl,
    write('Input: (A > 5 -> B = big)'), nl,
    write('Output:'), nl,
    starlog_output_code((A > 5 -> B = big)),
    nl.

% Test 6: Findall with simple goal
test_findall_simple :-
    write('Test 6: Findall with simple goal'), nl,
    write('Input: findall(X, member(X, [1,2,3]), Result)'), nl,
    write('Output:'), nl,
    starlog_output_code(findall(X, member(X, [1,2,3]), Result)),
    nl.

% Test 7: Findall with conjunction
test_findall_conjunction :-
    write('Test 7: Findall with conjunction'), nl,
    write('Input: findall(X, (member(X, [1,2,3]), X > 1), Result)'), nl,
    write('Output:'), nl,
    starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result)),
    nl.

% Test 8: Findall with if-then-else
test_findall_complex :-
    write('Test 8: Findall with if-then-else'), nl,
    write('Input: findall(X, (member(X, [1,2,3]), (X > 1 -> Y = big ; Y = small)), Result)'), nl,
    write('Output:'), nl,
    starlog_output_code(findall(X, (member(X, [1,2,3]), (X > 1 -> Y = big ; Y = small)), Result)),
    nl.

% Test 9: Nested control structures
test_nested_control :-
    write('Test 9: Nested control structures'), nl,
    write('Input: (A = 1, (B > 5 -> C = big ; C = small), D = 2)'), nl,
    write('Output:'), nl,
    starlog_output_code((A = 1, (B > 5 -> C = big ; C = small), D = 2)),
    nl.

% Run all tests
run_all_tests :-
    write('=== Running Pretty Print Tests ==='), nl, nl,
    test_conjunction,
    test_disjunction,
    test_negation_simple,
    test_if_then_else,
    test_if_then,
    test_findall_simple,
    test_findall_conjunction,
    test_findall_complex,
    test_nested_control,
    write('=== All tests complete ==='), nl.

:- initialization(run_all_tests, main).
