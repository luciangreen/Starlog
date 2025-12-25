% test_no_eval.pl
% Tests for no_eval functionality in Starlog

:- use_module('../starlog_in_prolog').

% Test no_eval with arithmetic
test_no_eval_arithmetic :-
    A is no_eval(1+1),
    A = 1+1,
    write('✓ no_eval with arithmetic test passed'), nl.

% Test no_eval with string concatenation operator
test_no_eval_string_concat :-
    B is no_eval("x":"y"),
    B = ("x":"y"),
    write('✓ no_eval with string concat test passed'), nl.

% Test no_eval with list append operator
test_no_eval_list_append :-
    C is no_eval([1] & [2]),
    C = ([1] & [2]),
    write('✓ no_eval with list append test passed'), nl.

% Test no_eval with atom concatenation operator
test_no_eval_atom_concat :-
    D is no_eval(hello • world),
    D = (hello • world),
    write('✓ no_eval with atom concat test passed'), nl.

% Test no_eval with simple value
test_no_eval_simple :-
    E is no_eval(hello),
    E = hello,
    write('✓ no_eval with simple value test passed'), nl.

% Test no_eval with complex expression
test_no_eval_complex :-
    F is no_eval((1+2)*3),
    F = ((1+2)*3),
    write('✓ no_eval with complex expression test passed'), nl.

% Test normal evaluation still works
test_normal_eval_still_works :-
    G is 1+1,
    G = 2,
    write('✓ Normal evaluation still works test passed'), nl.

% Test string concat still works normally
test_string_concat_still_works :-
    H is "x":"y",
    H = "xy",
    write('✓ String concat still works test passed'), nl.

% Run all tests
run_tests :-
    write('Running no_eval tests...'), nl, nl,
    catch(test_no_eval_arithmetic, E1, (write('✗ no_eval arithmetic failed: '), write(E1), nl)),
    catch(test_no_eval_string_concat, E2, (write('✗ no_eval string concat failed: '), write(E2), nl)),
    catch(test_no_eval_list_append, E3, (write('✗ no_eval list append failed: '), write(E3), nl)),
    catch(test_no_eval_atom_concat, E4, (write('✗ no_eval atom concat failed: '), write(E4), nl)),
    catch(test_no_eval_simple, E5, (write('✗ no_eval simple failed: '), write(E5), nl)),
    catch(test_no_eval_complex, E6, (write('✗ no_eval complex failed: '), write(E6), nl)),
    catch(test_normal_eval_still_works, E7, (write('✗ Normal eval failed: '), write(E7), nl)),
    catch(test_string_concat_still_works, E8, (write('✗ String concat failed: '), write(E8), nl)),
    nl,
    write('no_eval tests complete!'), nl.

:- initialization(run_tests, main).
