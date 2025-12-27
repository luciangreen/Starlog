% test_arithmetic_in_builtins.pl
% Tests for arithmetic expressions inside builtin function arguments
% This tests the requirement: A is number_string(2^2):2

:- use_module('../starlog').

% Test 1: Basic arithmetic in number_string
test_number_string_arithmetic :-
    write('Test 1: A is number_string(2^2)...'),
    starlog_call(A is number_string(2^2)),
    write(' A = '), write(A),
    (   A = "4"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "4"'), nl, fail
    ).

% Test 2: number_string with arithmetic concatenated with string
test_number_string_concat_string :-
    write('Test 2: A is number_string(2^2) : 2...'),
    starlog_call(A is number_string(2^2) : 2),
    write(' A = '), write(A),
    (   A = "42"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "42"'), nl, fail
    ).

% Test 3: More complex arithmetic
test_number_string_complex_arithmetic :-
    write('Test 3: A is number_string(3*4+5)...'),
    starlog_call(A is number_string(3*4+5)),
    write(' A = '), write(A),
    (   A = "17"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "17"'), nl, fail
    ).

% Test 4: Arithmetic in other builtins - string_length
test_arithmetic_in_string_length :-
    write('Test 4: A is string_length(number_string(10+5))...'),
    starlog_call(A is string_length(number_string(10+5))),
    write(' A = '), write(A),
    (   A = 2  % "15" has length 2
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected 2'), nl, fail
    ).

% Test 5: Multiple concatenations with arithmetic
test_multiple_concat_arithmetic :-
    write('Test 5: A is number_string(1+1) : number_string(2*2)...'),
    starlog_call(A is number_string(1+1) : number_string(2*2)),
    write(' A = '), write(A),
    (   A = "24"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "24"'), nl, fail
    ).

% Test 6: Nested arithmetic in reverse
test_arithmetic_in_reverse :-
    write('Test 6: Test arithmetic doesn\'t break other builtins...'),
    starlog_call(A is reverse([1,2,3])),
    write(' A = '), write(A),
    (   A = [3,2,1]
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected [3,2,1]'), nl, fail
    ).

% Test 7: Division and float arithmetic
test_number_string_division :-
    write('Test 7: A is number_string(10/2)...'),
    starlog_call(A is number_string(10/2)),
    write(' A = '), write(A),
    (   A = "5"  % Prolog simplifies 5.0 to 5 when converting to string
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "5"'), nl, fail
    ).

% Test 8: Power operator
test_number_string_power :-
    write('Test 8: A is number_string(3^3)...'),
    starlog_call(A is number_string(3^3)),
    write(' A = '), write(A),
    (   A = "27"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "27"'), nl, fail
    ).

% Test 9: Compound test from problem statement
test_problem_statement :-
    write('Test 9: Problem statement - A is number_string(2^2):2...'),
    starlog_call(A is number_string(2^2):2),
    write(' A = '), write(A),
    (   A = "42"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "42"'), nl, fail
    ).

% Test 10: Ensure non-arithmetic still works
test_non_arithmetic_unchanged :-
    write('Test 10: Non-arithmetic expressions still work...'),
    starlog_call(A is number_string(42)),
    write(' A = '), write(A),
    (   A = "42"
    ->  write(' ✓ PASS'), nl
    ;   write(' ✗ FAIL: Expected "42"'), nl, fail
    ).

% Run all tests
run_tests :-
    write('=========================================================='), nl,
    write('Testing Arithmetic Expressions in Builtin Arguments'), nl,
    write('=========================================================='), nl, nl,
    
    catch(test_number_string_arithmetic, E1, 
          (write('✗ Test 1 failed with exception: '), write(E1), nl, fail)),
    catch(test_number_string_concat_string, E2, 
          (write('✗ Test 2 failed with exception: '), write(E2), nl, fail)),
    catch(test_number_string_complex_arithmetic, E3, 
          (write('✗ Test 3 failed with exception: '), write(E3), nl, fail)),
    catch(test_arithmetic_in_string_length, E4, 
          (write('✗ Test 4 failed with exception: '), write(E4), nl, fail)),
    catch(test_multiple_concat_arithmetic, E5, 
          (write('✗ Test 5 failed with exception: '), write(E5), nl, fail)),
    catch(test_arithmetic_in_reverse, E6, 
          (write('✗ Test 6 failed with exception: '), write(E6), nl, fail)),
    catch(test_number_string_division, E7, 
          (write('✗ Test 7 failed with exception: '), write(E7), nl, fail)),
    catch(test_number_string_power, E8, 
          (write('✗ Test 8 failed with exception: '), write(E8), nl, fail)),
    catch(test_problem_statement, E9, 
          (write('✗ Test 9 failed with exception: '), write(E9), nl, fail)),
    catch(test_non_arithmetic_unchanged, E10, 
          (write('✗ Test 10 failed with exception: '), write(E10), nl, fail)),
    
    nl,
    write('=========================================================='), nl,
    write('All tests passed! ✓'), nl,
    write('=========================================================='), nl.

:- initialization(run_tests, main).
