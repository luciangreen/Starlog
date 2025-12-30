% test_findall_output_code.pl
% Tests for findall integration with starlog_output_code and starlog_call
% This tests the fix for: "Please fix all combinations and configurations of 
% starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result),C),starlog_call(C)."

:- use_module('../starlog').

test_basic_pattern :-
    write('Test 1: Basic pattern from problem statement'), nl,
    starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [2,3] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_with_print_false :-
    write('Test 2: With print(false) option'), nl,
    starlog_output_code(findall(Y, (member(Y, [1,2,3]), Y > 1), Result), C, [print(false)]),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [2,3] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_with_rename_false :-
    write('Test 3: With rename(false) option'), nl,
    starlog_output_code(findall(Z, (member(Z, [1,2,3]), Z > 1), Result), C, [rename(false)]),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [2,3] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_with_compress :-
    write('Test 4: With compress(true) option'), nl,
    starlog_output_code(findall(W, (member(W, [1,2,3]), W > 1), Result), C, [compress(true)]),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [2,3] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_different_goal :-
    write('Test 5: Different goal (no condition)'), nl,
    starlog_output_code(findall(A, member(A, [a,b,c]), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [a,b,c] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_nested_conditions :-
    write('Test 6: Multiple conditions in goal'), nl,
    starlog_output_code(findall(B, (member(B, [1,2,3,4,5]), B > 2, B < 5), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [3,4] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_empty_result :-
    write('Test 7: Empty result set'), nl,
    starlog_output_code(findall(D, (member(D, [1,2,3]), D > 10), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_complex_template :-
    write('Test 8: Complex template (pair)'), nl,
    starlog_output_code(findall([E,F], (member(E, [1,2,3]), F is E*2), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [[1,2],[2,4],[3,6]] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_with_starlog_expr :-
    write('Test 9: Goal contains starlog_call'), nl,
    starlog_output_code(findall(X, starlog_call(X is "a":"b"), Result), C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = ["ab"] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_multiple_sequential :-
    write('Test 10: Multiple sequential calls'), nl,
    starlog_output_code(findall(Y1, member(Y1, [1,2]), R1), C1),
    starlog_call(C1),
    starlog_output_code(findall(Y2, member(Y2, [3,4]), R2), C2),
    starlog_call(C2),
    write('  Result1: '), write(R1), nl,
    write('  Result2: '), write(R2), nl,
    (R1 = [1,2], R2 = [3,4] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_all_options :-
    write('Test 11: All options combined'), nl,
    starlog_output_code(
        findall(V, member(V, [p,q,r]), Result), 
        C, 
        [compress(true), print(false), rename(false)]
    ),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [p,q,r] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

test_nested_findall :-
    write('Test 12: Nested findall'), nl,
    Goal = findall(Outer, (member(Outer, [1,2]), findall(Inner, member(Inner, [a,b]), _InnerList)), Result),
    starlog_output_code(Goal, C),
    starlog_call(C),
    write('  Result: '), write(Result), nl,
    (Result = [1,2] -> 
        write('  ✓ PASS') 
    ; 
        write('  ✗ FAIL')
    ), nl, nl.

run_all_tests :-
    write('========================================'), nl,
    write('Testing findall with starlog_output_code'), nl,
    write('Problem: starlog_output_code(findall(...), C), starlog_call(C)'), nl,
    write('========================================'), nl, nl,
    
    test_basic_pattern,
    test_with_print_false,
    test_with_rename_false,
    test_with_compress,
    test_different_goal,
    test_nested_conditions,
    test_empty_result,
    test_complex_template,
    test_with_starlog_expr,
    test_multiple_sequential,
    test_all_options,
    test_nested_findall,
    
    write('========================================'), nl,
    write('All findall tests completed!'), nl,
    write('========================================'), nl.

:- initialization(run_all_tests, main).
