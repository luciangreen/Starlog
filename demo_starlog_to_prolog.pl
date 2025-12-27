% demo_starlog_to_prolog.pl
% Comprehensive demonstration of the Starlog to Prolog conversion feature

:- use_module(starlog).

main :-
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Starlog to Prolog Conversion Feature Demonstration      ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl, nl,
    
    demo_individual_goals,
    nl,
    demo_file_conversion,
    nl,
    demo_variable_naming,
    
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Demonstration Complete!                                  ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl.

% Demo 1: Converting individual Starlog goals to Prolog
demo_individual_goals :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 1: Converting Starlog Goals to Prolog Notation'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('1. String concatenation:'), nl,
    write('   Starlog: A is "Hello":"World"'), nl,
    write('   Prolog:  '),
    starlog_to_prolog_code(A is "Hello":"World"),
    nl,
    
    write('2. List append:'), nl,
    write('   Starlog: A is [1,2]&[3,4]'), nl,
    write('   Prolog:  '),
    starlog_to_prolog_code(A is [1,2]&[3,4]),
    nl,
    
    write('3. Atom concatenation:'), nl,
    write('   Starlog: A is hello•world'), nl,
    write('   Prolog:  '),
    starlog_to_prolog_code(A is hello•world),
    nl,
    
    write('4. Nested expression (decompressed):'), nl,
    write('   Starlog: A is reverse([1,2]&[3,4])'), nl,
    write('   Prolog:  '),
    starlog_to_prolog_code(A is reverse([1,2]&[3,4])),
    nl,
    
    write('5. Multiple nested operations:'), nl,
    write('   Starlog: A is "hello":" ":"world"'), nl,
    write('   Prolog:  '),
    starlog_to_prolog_code(A is "hello":" ":"world"),
    nl.

% Demo 2: Converting a file
demo_file_conversion :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 2: Converting a Starlog File to Prolog'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Converting tests/sample_starlog.pl...'), nl, nl,
    starlog_to_prolog_file('tests/sample_starlog.pl').

% Demo 3: Variable naming with maximal decompression
demo_variable_naming :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 3: Maximal Decompression with Human-Friendly Variables'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Complex nested Starlog expression:'), nl,
    write('  Starlog: Result is "Hello, ":First:" ":Last'), nl, nl,
    write('Decompressed Prolog:'), nl,
    write('  '),
    starlog_to_prolog_code(Result is "Hello, ":First:" ":Last),
    nl,
    write('Note: Variables are renamed to A, B, C, D... for readability'), nl,
    write('      Nested expressions are flattened into sequential goals'), nl.

:- initialization(main, main).
