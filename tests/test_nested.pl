% test_nested.pl
% Tests for nested Starlog expressions

:- use_module('../starlog_in_prolog').

% Test nested expression decompression
test_nested_string_concat :-
    starlog_call((E is ("a":"b"):"c")),
    E = "abc",
    write('✓ Nested string concat test passed'), nl.

% Test nested with atom concat
test_nested_atom_concat :-
    starlog_call((E is (a • b) • c)),
    E = abc,
    write('✓ Nested atom concat test passed'), nl.

% Test mixed operators
test_mixed_operators :-
    starlog_call((E is ("x":"y") • atom)),
    atom_string(E, S),
    S = "xyatom",
    write('✓ Mixed operators test passed'), nl.

% Test nested with builtin
test_nested_builtin :-
    starlog_call((L is string_length("a":"b"))),
    L = 2,
    write('✓ Nested with builtin test passed'), nl.

% Test complex nesting
test_complex_nesting :-
    starlog_call((R is reverse([1] & [2,3]))),
    R = [3,2,1],
    write('✓ Complex nesting test passed'), nl.

% Run all tests
run_tests :-
    write('Running nested expression tests...'), nl, nl,
    catch(test_nested_string_concat, E, (write('✗ Nested string concat failed: '), write(E), nl)),
    catch(test_nested_atom_concat, E2, (write('✗ Nested atom concat failed: '), write(E2), nl)),
    catch(test_mixed_operators, E3, (write('✗ Mixed operators failed: '), write(E3), nl)),
    catch(test_nested_builtin, E4, (write('✗ Nested builtin failed: '), write(E4), nl)),
    catch(test_complex_nesting, E5, (write('✗ Complex nesting failed: '), write(E5), nl)),
    nl,
    write('Nested expression tests complete!'), nl.

:- initialization(run_tests, main).
