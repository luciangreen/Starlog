% test_starlog_to_prolog.pl
% Tests for starlog_to_prolog_code functionality

:- use_module('../starlog').

% Test 1: Simple string concatenation
test_simple_concat :-
    write('Test 1: Simple string concatenation'), nl,
    write('Input Starlog: A is "hello":"world"'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code(A is "hello":"world"),
    nl.

% Test 2: List append
test_list_append :-
    write('Test 2: List append'), nl,
    write('Input Starlog: A is [1,2]&[3,4]'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code(A is [1,2]&[3,4]),
    nl.

% Test 3: Atom concatenation
test_atom_concat :-
    write('Test 3: Atom concatenation'), nl,
    write('Input Starlog: A is hello•world'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code(A is hello•world),
    nl.

% Test 4: Nested expression decompression
test_nested_decompress :-
    write('Test 4: Nested expression decompression'), nl,
    write('Input Starlog: A is reverse([1,2]&[3,4])'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code(A is reverse([1,2]&[3,4])),
    nl.

% Test 5: Multiple nested operations
test_multiple_nested :-
    write('Test 5: Multiple nested operations'), nl,
    write('Input Starlog: A is "hello":" ":"world"'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code(A is "hello":" ":"world"),
    nl.

% Test 6: Complex clause with Starlog body
test_complex_clause :-
    write('Test 6: Complex clause'), nl,
    write('Input: greet(X,Y,Z) :- Z is "Hello, ":X:" ":Y'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code((Z is "Hello, ":X:" ":Y)),
    nl.

% Test 7: Multiple goals with variable reuse
test_multiple_goals :-
    write('Test 7: Multiple goals'), nl,
    write('Input: (A is "hello":"world", B is reverse(A))'), nl,
    write('Output Prolog: '),
    starlog_to_prolog_code((A is "hello":"world", B is reverse(A))),
    nl.

% Run all tests
run_all_tests :-
    write('=== Running Starlog to Prolog Conversion Tests ==='), nl, nl,
    test_simple_concat,
    test_list_append,
    test_atom_concat,
    test_nested_decompress,
    test_multiple_nested,
    test_complex_clause,
    test_multiple_goals,
    write('=== All tests complete ==='), nl.

:- initialization(run_all_tests, main).
