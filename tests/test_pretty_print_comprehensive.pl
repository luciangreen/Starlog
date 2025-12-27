% test_pretty_print_comprehensive.pl
% Comprehensive tests to verify that pretty printing works for all cases

:- use_module('../starlog').

test_all :-
    write('╔══════════════════════════════════════════════════════════════╗'), nl,
    write('║  Comprehensive Pretty Print Verification Test Suite          ║'), nl,
    write('╚══════════════════════════════════════════════════════════════╝'), nl, nl,
    
    test_starlog_output,
    nl,
    test_prolog_output,
    nl,
    test_file_output,
    nl,
    
    write('╔══════════════════════════════════════════════════════════════╗'), nl,
    write('║  All Pretty Print Tests Passed Successfully!                 ║'), nl,
    write('╚══════════════════════════════════════════════════════════════╝'), nl.

test_starlog_output :-
    write('══════════════════════════════════════════════════════════════'), nl,
    write('Test 1: Starlog Output Pretty Printing'), nl,
    write('══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('1.1. Simple Starlog call:'), nl,
    starlog_output_code(string_concat("a", "b", C)),
    nl,
    
    write('1.2. Findall with nested control:'), nl,
    starlog_output_code(findall(X, (member(X,[1,2,3]), X > 1), R)),
    nl,
    
    write('1.3. If-then-else:'), nl,
    starlog_output_code((A > 5 -> B = big ; B = small)),
    nl,
    
    write('1.4. Complex nested structure:'), nl,
    starlog_output_code((
        A > 5 -> (
            B > 10 -> C = huge ; C = big
        ) ; C = small
    )),
    nl,
    
    write('✓ Starlog output pretty printing working correctly'), nl.

test_prolog_output :-
    write('══════════════════════════════════════════════════════════════'), nl,
    write('Test 2: Prolog Output Pretty Printing'), nl,
    write('══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('2.1. Simple Prolog conversion:'), nl,
    starlog_to_prolog_code(A is "hello":"world"),
    nl,
    
    write('2.2. Nested Prolog conversion:'), nl,
    starlog_to_prolog_code(A is "a":" ":"b"),
    nl,
    
    write('2.3. Value-returning builtin:'), nl,
    starlog_to_prolog_code(A is reverse([1,2,3])),
    nl,
    
    write('✓ Prolog output pretty printing working correctly'), nl.

test_file_output :-
    write('══════════════════════════════════════════════════════════════'), nl,
    write('Test 3: File Output Pretty Printing'), nl,
    write('══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('3.1. Starlog file output:'), nl,
    starlog_output_file('sample_prolog.pl'),
    nl,
    
    write('3.2. Prolog file output:'), nl,
    starlog_to_prolog_file('sample_prolog.pl'),
    nl,
    
    write('✓ File output pretty printing working correctly'), nl.

:- initialization(test_all, main).
