% test_call_to_variable.pl
% Tests for saving Prolog and Starlog calls to variables

:- use_module('../starlog_in_prolog').

% Test 1: starlog_call/2 with string concatenation
test_call_2_string_concat :-
    starlog_call(X is "hello":"world", Result),
    Result = "helloworld",
    write('✓ starlog_call/2 with string concat test passed'), nl.

% Test 2: starlog_call/2 with list append
test_call_2_list_append :-
    starlog_call(Y is [1,2] & [3,4], Result),
    Result = [1,2,3,4],
    write('✓ starlog_call/2 with list append test passed'), nl.

% Test 3: starlog_call/2 with atom concatenation
test_call_2_atom_concat :-
    starlog_call(Z is hello • world, Result),
    Result = helloworld,
    write('✓ starlog_call/2 with atom concat test passed'), nl.

% Test 4: starlog_call/2 with value-returning builtin
test_call_2_builtin :-
    starlog_call(L is reverse([1,2,3]), Result),
    Result = [3,2,1],
    write('✓ starlog_call/2 with builtin test passed'), nl.

% Test 5: starlog_call/2 with arithmetic
test_call_2_arithmetic :-
    starlog_call(N is 5 + 3, Result),
    Result = 8,
    write('✓ starlog_call/2 with arithmetic test passed'), nl.

% Test 6: starlog_eval/2 with string concatenation
test_eval_string_concat :-
    starlog_eval("x":"y", Result),
    Result = "xy",
    write('✓ starlog_eval/2 with string concat test passed'), nl.

% Test 7: starlog_eval/2 with arithmetic
test_eval_arithmetic :-
    starlog_eval(2+2, Result),
    Result = 4,
    write('✓ starlog_eval/2 with arithmetic test passed'), nl.

% Test 8: starlog_eval/2 with list append
test_eval_list_append :-
    starlog_eval([1] & [2,3], Result),
    Result = [1,2,3],
    write('✓ starlog_eval/2 with list append test passed'), nl.

% Test 9: starlog_no_eval/2 with arithmetic (should preserve)
test_no_eval_arithmetic :-
    starlog_no_eval(1+1, Result),
    Result = 1+1,
    write('✓ starlog_no_eval/2 with arithmetic test passed'), nl.

% Test 10: starlog_no_eval/2 with string concatenation (should preserve)
test_no_eval_string_concat :-
    starlog_no_eval("hello":"world", Result),
    Result = "hello":"world",
    write('✓ starlog_no_eval/2 with string concat test passed'), nl.

% Test 11: starlog_no_eval/2 with list append (should preserve)
test_no_eval_list_append :-
    starlog_no_eval([a,b] & [c,d], Result),
    Result = [a,b] & [c,d],
    write('✓ starlog_no_eval/2 with list append test passed'), nl.

% Test 12: starlog_no_eval/2 with complex expression
test_no_eval_complex :-
    starlog_no_eval((1+2)*(3+4), Result),
    Result = (1+2)*(3+4),
    write('✓ starlog_no_eval/2 with complex expression test passed'), nl.

% Test 13: starlog_eval/2 with nested expression
test_eval_nested :-
    starlog_eval("a":"b":"c", Result),
    Result = "abc",
    write('✓ starlog_eval/2 with nested expression test passed'), nl.

% Test 14: Combining starlog_call/2 with eval
test_call_2_with_eval :-
    starlog_call(X is eval(1+1), Result),
    Result = 2,
    write('✓ starlog_call/2 with eval test passed'), nl.

% Test 15: Combining starlog_call/2 with no_eval
test_call_2_with_no_eval :-
    starlog_call(Y is no_eval(2+2), Result),
    Result = 2+2,
    write('✓ starlog_call/2 with no_eval test passed'), nl.

% Test 16: starlog_eval inside no_eval context
test_eval_in_no_eval :-
    starlog_call(Z is no_eval(eval(1+1)), Result),
    Result = 2,
    write('✓ eval inside no_eval test passed'), nl.

% Test 17: Multiple operations with starlog_call/2
test_call_2_multiple :-
    starlog_call(A is "hello":" ", R1),
    starlog_call(B is R1:"world", R2),
    R2 = "hello world",
    write('✓ starlog_call/2 with multiple operations test passed'), nl.

% Test 18: starlog_no_eval preserving nested structure
test_no_eval_nested_structure :-
    starlog_no_eval([1, 2+2, 3], Result),
    Result = [1, 2+2, 3],
    write('✓ starlog_no_eval/2 preserving nested structure test passed'), nl.

% Run all tests
run_tests :-
    write('Running call-to-variable tests...'), nl, nl,
    catch(test_call_2_string_concat, E1, (write('✗ test_call_2_string_concat failed: '), write(E1), nl)),
    catch(test_call_2_list_append, E2, (write('✗ test_call_2_list_append failed: '), write(E2), nl)),
    catch(test_call_2_atom_concat, E3, (write('✗ test_call_2_atom_concat failed: '), write(E3), nl)),
    catch(test_call_2_builtin, E4, (write('✗ test_call_2_builtin failed: '), write(E4), nl)),
    catch(test_call_2_arithmetic, E5, (write('✗ test_call_2_arithmetic failed: '), write(E5), nl)),
    catch(test_eval_string_concat, E6, (write('✗ test_eval_string_concat failed: '), write(E6), nl)),
    catch(test_eval_arithmetic, E7, (write('✗ test_eval_arithmetic failed: '), write(E7), nl)),
    catch(test_eval_list_append, E8, (write('✗ test_eval_list_append failed: '), write(E8), nl)),
    catch(test_no_eval_arithmetic, E9, (write('✗ test_no_eval_arithmetic failed: '), write(E9), nl)),
    catch(test_no_eval_string_concat, E10, (write('✗ test_no_eval_string_concat failed: '), write(E10), nl)),
    catch(test_no_eval_list_append, E11, (write('✗ test_no_eval_list_append failed: '), write(E11), nl)),
    catch(test_no_eval_complex, E12, (write('✗ test_no_eval_complex failed: '), write(E12), nl)),
    catch(test_eval_nested, E13, (write('✗ test_eval_nested failed: '), write(E13), nl)),
    catch(test_call_2_with_eval, E14, (write('✗ test_call_2_with_eval failed: '), write(E14), nl)),
    catch(test_call_2_with_no_eval, E15, (write('✗ test_call_2_with_no_eval failed: '), write(E15), nl)),
    catch(test_eval_in_no_eval, E16, (write('✗ test_eval_in_no_eval failed: '), write(E16), nl)),
    catch(test_call_2_multiple, E17, (write('✗ test_call_2_multiple failed: '), write(E17), nl)),
    catch(test_no_eval_nested_structure, E18, (write('✗ test_no_eval_nested_structure failed: '), write(E18), nl)),
    nl,
    write('Call-to-variable tests complete!'), nl.

:- initialization(run_tests, main).
