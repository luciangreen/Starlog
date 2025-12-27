% test_output_to_variable.pl
% Test that starlog_output_code and starlog_to_prolog_code can output to variables without printing

:- use_module(starlog).

test_starlog_output_code_to_variable :-
    write('Test 1: starlog_output_code/2 should return code without printing'), nl,
    write('Calling: starlog_output_code(string_concat("x", "y", C), Code)'), nl,
    starlog_output_code(string_concat("x", "y", C), Code),
    write('Code variable contains: '), write(Code), nl,
    (Code = (_ is _) -> 
        write('SUCCESS: Code is in Starlog form (contains is operator)'), nl
    ;
        write('FAILURE: Code is not in expected form'), nl
    ),
    nl.

test_starlog_to_prolog_code_to_variable :-
    write('Test 2: starlog_to_prolog_code/2 should return code without printing'), nl,
    write('Calling: starlog_to_prolog_code(A is "hello":"world", Code)'), nl,
    starlog_to_prolog_code(A is "hello":"world", Code),
    write('Code variable contains: '), write(Code), nl,
    (Code = string_concat(_, _, _) -> 
        write('SUCCESS: Code is in Prolog form (contains string_concat)'), nl
    ;
        write('FAILURE: Code is not in expected form'), nl
    ),
    nl.

test_starlog_output_code_with_print :-
    write('Test 3: starlog_output_code/1 should still print to stdout'), nl,
    write('Calling: starlog_output_code(string_concat("x", "y", C))'), nl,
    write('Output: '),
    starlog_output_code(string_concat("x", "y", C)),
    nl.

test_starlog_to_prolog_code_with_print :-
    write('Test 4: starlog_to_prolog_code/1 should still print to stdout'), nl,
    write('Calling: starlog_to_prolog_code(A is "hello":"world")'), nl,
    write('Output: '),
    starlog_to_prolog_code(A is "hello":"world"),
    nl.

test_starlog_output_code_with_options :-
    write('Test 5: starlog_output_code/3 with print(false) should not print'), nl,
    write('Calling: starlog_output_code(string_concat("x", "y", C), Code, [print(false)])'), nl,
    starlog_output_code(string_concat("x", "y", C), Code, [print(false)]),
    write('Code variable contains: '), write(Code), nl,
    nl.

test_starlog_to_prolog_code_with_options :-
    write('Test 6: starlog_to_prolog_code/3 with print(false) should not print'), nl,
    write('Calling: starlog_to_prolog_code(A is "hello":"world", Code, [print(false)])'), nl,
    starlog_to_prolog_code(A is "hello":"world", Code, [print(false)]),
    write('Code variable contains: '), write(Code), nl,
    nl.

run_all_tests :-
    write('╔═══════════════════════════════════════════════════════════╗'), nl,
    write('║   Testing Output to Variables Feature                     ║'), nl,
    write('╚═══════════════════════════════════════════════════════════╝'), nl, nl,
    test_starlog_output_code_to_variable,
    test_starlog_to_prolog_code_to_variable,
    test_starlog_output_code_with_print,
    test_starlog_to_prolog_code_with_print,
    test_starlog_output_code_with_options,
    test_starlog_to_prolog_code_with_options,
    write('╔═══════════════════════════════════════════════════════════╗'), nl,
    write('║   All Tests Complete!                                      ║'), nl,
    write('╚═══════════════════════════════════════════════════════════╝'), nl.

:- initialization(run_all_tests, main).
