% test_output_code.pl
% Tests for starlog_output_code functionality

:- use_module('../starlog_in_prolog').

% Test 1: Output simple string concatenation
test_output_simple_concat :-
    write('Test 1: Simple string concatenation'), nl,
    write('Input Prolog: string_concat("x", "y", C)'), nl,
    write('Output Starlog: '),
    starlog_output_code(string_concat("x", "y", C)),
    nl.

% Test 2: Output list append
test_output_list_append :-
    write('Test 2: List append'), nl,
    write('Input Prolog: append([1], [2], L)'), nl,
    write('Output Starlog: '),
    starlog_output_code(append([1], [2], L)),
    nl.

% Test 3: Output atom concatenation
test_output_atom_concat :-
    write('Test 3: Atom concatenation'), nl,
    write('Input Prolog: atom_concat(hello, world, A)'), nl,
    write('Output Starlog: '),
    starlog_output_code(atom_concat(hello, world, A)),
    nl.

% Test 4: Output value-returning builtin
test_output_value_builtin :-
    write('Test 4: Value-returning builtin (reverse)'), nl,
    write('Input Prolog: reverse([1,2,3], R)'), nl,
    write('Output Starlog: '),
    starlog_output_code(reverse([1,2,3], R)),
    nl.

% Test 5: Output already Starlog code (should preserve)
test_output_starlog_code :-
    write('Test 5: Already Starlog code'), nl,
    write('Input Starlog: A is "x":"y"'), nl,
    write('Output Starlog: '),
    starlog_output_code(A is "x":"y"),
    nl.

% Test 6: Multiple variables with human-friendly names
test_output_multiple_vars :-
    write('Test 6: Multiple variables'), nl,
    write('Input Prolog: string_concat(X, Y, Z), append(Z, W, R)'), nl,
    write('Output Starlog: '),
    starlog_output_code((string_concat(X, Y, Z), append(Z, W, R))),
    nl.

% Run all tests
run_all_tests :-
    write('=== Running Starlog Output Code Tests ==='), nl, nl,
    test_output_simple_concat,
    test_output_list_append,
    test_output_atom_concat,
    test_output_value_builtin,
    test_output_starlog_code,
    test_output_multiple_vars,
    write('=== All tests complete ==='), nl.

:- initialization(run_all_tests, main).
