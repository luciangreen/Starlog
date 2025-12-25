% test_number_string_number.pl
% Tests for number(string_number(...)) expressions
% Validates the requirements:
% 1. A is number(string_number(1:1)). A = number(11). should be true
% 2. A is number(string_number(a:a)). A = number(11). should be false

:- use_module('../starlog_in_prolog').

% Test 1: number(string_number(1:1)) should result in number(11)
test_number_string_number_numeric :-
    write('Test 1: A is number(string_number(1:1))'), nl,
    starlog_call(A is number(string_number(1:1))),
    write('  A = '), write(A), nl,
    (   A = number(11)
    ->  write('  ✓ PASS: A = number(11) is true'), nl
    ;   write('  ✗ FAIL: A should be number(11) but got '), write(A), nl,
        fail
    ).

% Test 2: number(string_number(a:a)) should fail or not equal number(11)
test_number_string_number_non_numeric :-
    write('Test 2: A is number(string_number(a:a))'), nl,
    (   starlog_call(A is number(string_number(a:a)))
    ->  write('  A = '), write(A), nl,
        (   A = number(11)
        ->  write('  ✗ FAIL: A = number(11) should be false'), nl,
            fail
        ;   write('  ✓ PASS: A = number(11) is false (A = '), write(A), write(')'), nl
        )
    ;   write('  ✓ PASS: Expression failed as expected, A = number(11) is false'), nl
    ).

% Test 3: Verify that string_number itself works correctly
test_string_number_numeric :-
    write('Test 3: A is string_number(1:1)'), nl,
    starlog_call(A is string_number(1:1)),
    write('  A = '), write(A), nl,
    (   A = 11
    ->  write('  ✓ PASS: A = 11'), nl
    ;   write('  ✗ FAIL: A should be 11 but got '), write(A), nl,
        fail
    ).

% Test 4: Verify that string_number fails for non-numeric strings
test_string_number_non_numeric :-
    write('Test 4: A is string_number(a:a)'), nl,
    (   starlog_call(A is string_number(a:a))
    ->  write('  ✗ FAIL: Should have failed but got A = '), write(A), nl,
        fail
    ;   write('  ✓ PASS: Expression failed as expected'), nl
    ).

% Test 5: Additional test with different numeric values
test_number_string_number_other_numeric :-
    write('Test 5: A is number(string_number(4:2))'), nl,
    starlog_call(A is number(string_number(4:2))),
    write('  A = '), write(A), nl,
    (   A = number(42)
    ->  write('  ✓ PASS: A = number(42) is true'), nl
    ;   write('  ✗ FAIL: A should be number(42) but got '), write(A), nl,
        fail
    ).

% Run all tests
run_tests :-
    write('=============================================='), nl,
    write('Running number(string_number(...)) tests'), nl,
    write('=============================================='), nl, nl,
    catch(test_number_string_number_numeric, E1, 
          (write('✗ Test 1 failed with exception: '), write(E1), nl, fail)),
    nl,
    catch(test_number_string_number_non_numeric, E2, 
          (write('✗ Test 2 failed with exception: '), write(E2), nl, fail)),
    nl,
    catch(test_string_number_numeric, E3, 
          (write('✗ Test 3 failed with exception: '), write(E3), nl, fail)),
    nl,
    catch(test_string_number_non_numeric, E4, 
          (write('✗ Test 4 failed with exception: '), write(E4), nl, fail)),
    nl,
    catch(test_number_string_number_other_numeric, E5, 
          (write('✗ Test 5 failed with exception: '), write(E5), nl, fail)),
    nl,
    write('=============================================='), nl,
    write('All tests passed!'), nl,
    write('=============================================='), nl.

:- initialization(run_tests, main).
