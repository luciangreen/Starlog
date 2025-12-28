% Test for dual expression with nested list appends
% This test verifies that the fix for preventing infinite backtracking works correctly.

:- use_module('../starlog').

test_exact_problem_statement :-
    write('Test 1: Exact problem statement - [A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]]'), nl,
    starlog_call(([A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])),
    write('  A = '), write(A), nl,
    (A = [p] -> write('  ✓ PASS') ; write('  ✗ FAIL')), nl.

test_no_hang_with_findall :-
    write('Test 2: No hang with findall'), nl,
    findall(A, starlog_call(([A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])), Solutions),
    write('  Solutions: '), write(Solutions), nl,
    length(Solutions, Count),
    (Count =:= 1 -> write('  ✓ PASS - Single solution, no infinite backtracking') ; write('  ✗ FAIL - Multiple solutions')), nl.

test_multiple_calls :-
    write('Test 3: Multiple calls do not hang'), nl,
    starlog_call(([A1&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])),
    starlog_call(([A2&[a]&[b]] is [[q]&[a]&[b]])),
    starlog_call(([A3&[1]&[2]&[3]&[4]] is [[r]&[1]&[2]&[3]&[4]])),
    write('  A1 = '), write(A1), 
    write(', A2 = '), write(A2),
    write(', A3 = '), write(A3), nl,
    ((A1 = [p], A2 = [q], A3 = [r]) -> write('  ✓ PASS') ; write('  ✗ FAIL')), nl.

test_with_different_values :-
    write('Test 4: With different constant values'), nl,
    starlog_call(([A&[1]&[2]] is [[hello]&[1]&[2]])),
    write('  A = '), write(A), nl,
    (A = [hello] -> write('  ✓ PASS') ; write('  ✗ FAIL')), nl.

test_symmetric_case :-
    write('Test 5: Symmetric case (same on both sides)'), nl,
    starlog_call(([A&[x]&[y]] is [B&[x]&[y]])),
    write('  A = '), write(A), write(', B = '), write(B), nl,
    (A = B -> write('  ✓ PASS - Variables unified correctly') ; write('  ✗ FAIL')), nl.

run_all_tests :-
    write('===================================='), nl,
    write('Testing Dual Expression Fix'), nl,
    write('===================================='), nl, nl,
    test_exact_problem_statement, nl,
    test_no_hang_with_findall, nl,
    test_multiple_calls, nl,
    test_with_different_values, nl,
    test_symmetric_case, nl,
    write('===================================='), nl,
    write('All tests completed!'), nl,
    write('===================================='), nl.

:- run_all_tests.
:- halt.
