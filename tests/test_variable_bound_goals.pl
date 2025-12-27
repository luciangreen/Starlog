% test_variable_bound_goals.pl
% Tests for variable-bound Starlog goals
% This tests the feature that allows: A=(C is no_eval(eval(1+1))),A

:- use_module('../starlog').

% Test 1: Variable-bound goal with no_eval and eval
test_variable_bound_no_eval_eval :-
    A=(C is no_eval(eval(1+1))),A,
    C = 2,
    write('✓ Variable-bound no_eval(eval(...)) test passed'), nl.

% Test 2: Variable-bound goal with simple Starlog expression
test_variable_bound_simple :-
    B=(D is "hello":"world"),B,
    D = "helloworld",
    write('✓ Variable-bound simple Starlog expression test passed'), nl.

% Test 3: Variable-bound goal with list append
test_variable_bound_list_append :-
    E=(F is [1,2] & [3,4]),E,
    F = [1,2,3,4],
    write('✓ Variable-bound list append test passed'), nl.

% Test 4: Variable-bound goal followed by additional goals
test_variable_bound_with_continuation :-
    G=(H is no_eval(eval(2+2))),G,
    I is H + 1,
    I = 5,
    write('✓ Variable-bound goal with continuation test passed'), nl.

% Test 5: Multiple variable-bound goals
test_multiple_variable_bound :-
    J=(K is "a":"b"),J,
    L=(M is "c":"d"),L,
    N is K:M,
    N = "abcd",
    write('✓ Multiple variable-bound goals test passed'), nl.

% Test 6: Nested variable-bound goals
test_nested_variable_bound :-
    O=(P is no_eval("x" : eval("y":"z"))),O,
    P = "x":"yz",
    write('✓ Nested variable-bound goal test passed'), nl.

% Test 7: Variable-bound goal with complex expression
test_variable_bound_complex :-
    Q=(R is no_eval(eval([1] & [2]) & [3])),Q,
    R = [1,2] & [3],
    write('✓ Variable-bound complex expression test passed'), nl.

% Test 8: Variable-bound goal that's already expanded (normal is/2)
test_variable_bound_arithmetic :-
    S=(T is 2+2),S,
    T = 4,
    write('✓ Variable-bound arithmetic expression test passed'), nl.

% Run all tests
run_tests :-
    write('Running variable-bound goal tests...'), nl, nl,
    catch(test_variable_bound_no_eval_eval, E1, (write('✗ variable-bound no_eval(eval(...)) failed: '), write(E1), nl)),
    catch(test_variable_bound_simple, E2, (write('✗ variable-bound simple failed: '), write(E2), nl)),
    catch(test_variable_bound_list_append, E3, (write('✗ variable-bound list append failed: '), write(E3), nl)),
    catch(test_variable_bound_with_continuation, E4, (write('✗ variable-bound with continuation failed: '), write(E4), nl)),
    catch(test_multiple_variable_bound, E5, (write('✗ multiple variable-bound failed: '), write(E5), nl)),
    catch(test_nested_variable_bound, E6, (write('✗ nested variable-bound failed: '), write(E6), nl)),
    catch(test_variable_bound_complex, E7, (write('✗ variable-bound complex failed: '), write(E7), nl)),
    catch(test_variable_bound_arithmetic, E8, (write('✗ variable-bound arithmetic failed: '), write(E8), nl)),
    nl,
    write('Variable-bound goal tests complete!'), nl.

:- initialization(run_tests, main).
