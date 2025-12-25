% test_eval.pl
% Tests for eval functionality in Starlog

:- use_module('../starlog_in_prolog').

% Test 1: Basic eval with arithmetic (should evaluate like normal)
test_eval_arithmetic :-
    A is eval(1+1),
    A = 2,
    write('✓ eval with arithmetic test passed'), nl.

% Test 2: eval inside no_eval - should evaluate the inner expression
test_eval_inside_no_eval_arithmetic :-
    B is no_eval(eval(1+1)),
    B = 2,
    write('✓ eval inside no_eval with arithmetic test passed'), nl.

% Test 3: eval with Starlog string concatenation
test_eval_string_concat :-
    C is eval("x":"y"),
    C = "xy",
    write('✓ eval with string concat test passed'), nl.

% Test 4: eval inside no_eval with Starlog operators
test_eval_inside_no_eval_starlog :-
    D is no_eval(eval("x":"y")),
    D = "xy",
    write('✓ eval inside no_eval with starlog operators test passed'), nl.

% Test 5: Nested - eval inside compound expression inside no_eval
test_nested_eval_in_no_eval :-
    E is no_eval("a" : eval("b":"c")),
    E = "a":"bc",
    write('✓ nested eval in no_eval test passed'), nl.

% Test 6: Multiple eval inside no_eval
test_multiple_eval_in_no_eval :-
    F is no_eval(eval(1+1) + eval(2+2)),
    F = 2 + 4,
    write('✓ multiple eval in no_eval test passed'), nl.

% Test 7: eval with list append
test_eval_list_append :-
    G is eval([1] & [2]),
    G = [1, 2],
    write('✓ eval with list append test passed'), nl.

% Test 8: eval inside no_eval with list append
test_eval_inside_no_eval_list :-
    H is no_eval(eval([1] & [2])),
    H = [1, 2],
    write('✓ eval inside no_eval with list append test passed'), nl.

% Test 9: Complex nested case
test_complex_nested :-
    I is no_eval([1, eval(2+2), 5]),
    I = [1, 4, 5],
    write('✓ complex nested eval test passed'), nl.

% Test 10: Deeply nested eval
test_deeply_nested_eval :-
    J is no_eval(no_eval(eval(1+1))),
    J = no_eval(2),
    write('✓ deeply nested eval test passed'), nl.

% Test 11: eval with atom concatenation
test_eval_atom_concat :-
    K is eval(hello • world),
    K = helloworld,
    write('✓ eval with atom concat test passed'), nl.

% Test 12: eval preserves normal evaluation by default
test_default_eval :-
    % Without explicit eval, expressions should still evaluate (eval is default)
    L is "x":"y",
    L = "xy",
    write('✓ default evaluation (eval is default) test passed'), nl.

% Run all tests
run_tests :-
    write('Running eval tests...'), nl, nl,
    catch(test_eval_arithmetic, E1, (write('✗ eval arithmetic failed: '), write(E1), nl)),
    catch(test_eval_inside_no_eval_arithmetic, E2, (write('✗ eval inside no_eval arithmetic failed: '), write(E2), nl)),
    catch(test_eval_string_concat, E3, (write('✗ eval string concat failed: '), write(E3), nl)),
    catch(test_eval_inside_no_eval_starlog, E4, (write('✗ eval inside no_eval starlog failed: '), write(E4), nl)),
    catch(test_nested_eval_in_no_eval, E5, (write('✗ nested eval in no_eval failed: '), write(E5), nl)),
    catch(test_multiple_eval_in_no_eval, E6, (write('✗ multiple eval in no_eval failed: '), write(E6), nl)),
    catch(test_eval_list_append, E7, (write('✗ eval list append failed: '), write(E7), nl)),
    catch(test_eval_inside_no_eval_list, E8, (write('✗ eval inside no_eval list failed: '), write(E8), nl)),
    catch(test_complex_nested, E9, (write('✗ complex nested eval failed: '), write(E9), nl)),
    catch(test_deeply_nested_eval, E10, (write('✗ deeply nested eval failed: '), write(E10), nl)),
    catch(test_eval_atom_concat, E11, (write('✗ eval atom concat failed: '), write(E11), nl)),
    catch(test_default_eval, E12, (write('✗ default eval failed: '), write(E12), nl)),
    nl,
    write('eval tests complete!'), nl.

:- initialization(run_tests, main).
