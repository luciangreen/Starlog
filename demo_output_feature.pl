% demo_output_feature.pl
% Comprehensive demonstration of the Starlog code output feature

:- use_module(starlog_in_prolog).

main :-
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Starlog Code Output Feature Demonstration               ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl, nl,
    
    demo_output_code,
    nl,
    demo_file_conversion,
    nl,
    demo_variable_naming,
    
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Demonstration Complete!                                  ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl.

% Demo 1: Converting individual Prolog goals to Starlog
demo_output_code :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 1: Converting Prolog Goals to Starlog Notation'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('1. String concatenation:'), nl,
    write('   Prolog:  string_concat("Hello", "World", Result)'), nl,
    write('   Starlog: '),
    starlog_output_code(string_concat("Hello", "World", Result)),
    nl,
    
    write('2. List append:'), nl,
    write('   Prolog:  append([1,2], [3,4], List)'), nl,
    write('   Starlog: '),
    starlog_output_code(append([1,2], [3,4], List)),
    nl,
    
    write('3. Atom concatenation:'), nl,
    write('   Prolog:  atom_concat(hello, world, Atom)'), nl,
    write('   Starlog: '),
    starlog_output_code(atom_concat(hello, world, Atom)),
    nl,
    
    write('4. Value-returning builtin:'), nl,
    write('   Prolog:  reverse([a,b,c], Rev)'), nl,
    write('   Starlog: '),
    starlog_output_code(reverse([a,b,c], Rev)),
    nl,
    
    write('5. String length:'), nl,
    write('   Prolog:  string_length("test", Len)'), nl,
    write('   Starlog: '),
    starlog_output_code(string_length("test", Len)),
    nl.

% Demo 2: Converting a file
demo_file_conversion :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 2: Converting a Prolog File to Starlog'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Converting tests/sample_prolog.pl...'), nl, nl,
    starlog_output_file('tests/sample_prolog.pl').

% Demo 3: Variable naming with multiple variables
demo_variable_naming :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 3: Human-Friendly Variable Naming'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Complex goal with multiple variables:'), nl,
    write('Prolog:'), nl,
    write('  string_concat(X, Y, Z),'), nl,
    write('  append(Z, W, R),'), nl,
    write('  reverse(R, Final)'), nl, nl,
    write('Starlog: '),
    starlog_output_code((string_concat(X, Y, Z), append(Z, W, R), reverse(R, Final))),
    nl,
    write('Note: Variables are renamed to A, B, C, D, E, F for readability'), nl.

:- initialization(main, main).
