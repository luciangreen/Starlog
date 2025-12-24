% test_basic.pl
% Basic tests for Starlog-in-Prolog

:- use_module('../starlog_in_prolog').

% Test string concatenation
test_string_concat :-
    starlog_call((A is "x":"y")),
    A = "xy",
    write('✓ String concatenation test passed'), nl.

% Test list append
test_list_append :-
    starlog_call((A is [1] & [2])),
    A = [1,2],
    write('✓ List append test passed'), nl.

% Test atom concatenation
test_atom_concat :-
    starlog_call((A is hello • world)),
    A = helloworld,
    write('✓ Atom concatenation test passed'), nl.

% Test string length
test_string_length :-
    starlog_call((L is string_length("hello"))),
    L = 5,
    write('✓ String length test passed'), nl.

% Test arithmetic is preserved
test_arithmetic :-
    starlog_call((X is 1+2)),
    X = 3,
    write('✓ Arithmetic is test passed'), nl.

% Test value builtin
test_value_builtin :-
    starlog_call((L is reverse([1,2,3]))),
    L = [3,2,1],
    write('✓ Value builtin test passed'), nl.

% Run all tests
run_tests :-
    write('Running basic Starlog tests...'), nl, nl,
    catch(test_string_concat, E, (write('✗ String concat failed: '), write(E), nl)),
    catch(test_list_append, E2, (write('✗ List append failed: '), write(E2), nl)),
    catch(test_atom_concat, E3, (write('✗ Atom concat failed: '), write(E3), nl)),
    catch(test_string_length, E4, (write('✗ String length failed: '), write(E4), nl)),
    catch(test_arithmetic, E5, (write('✗ Arithmetic failed: '), write(E5), nl)),
    catch(test_value_builtin, E6, (write('✗ Value builtin failed: '), write(E6), nl)),
    nl,
    write('Basic tests complete!'), nl.

:- initialization(run_tests, main).
