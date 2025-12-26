% demo_variable_output.pl
% Demonstration of the new variable output feature
% Shows how starlog_output_code/2 and starlog_to_prolog_code/2
% can return code in variables without printing to stdout

:- use_module(starlog_in_prolog).

main :-
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Variable Output Feature Demonstration                   ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl, nl,
    
    demo_programmatic_use,
    nl,
    demo_code_manipulation,
    nl,
    demo_comparison,
    
    write('╔════════════════════════════════════════════════════════════╗'), nl,
    write('║   Demonstration Complete!                                  ║'), nl,
    write('╚════════════════════════════════════════════════════════════╝'), nl.

% Demo 1: Programmatic use - getting code for further processing
demo_programmatic_use :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 1: Programmatic Use - Code in Variables'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Converting Prolog to Starlog and storing in variable:'), nl,
    starlog_output_code(string_concat("hello", "world", R), StarlogCode),
    write('  StarlogCode = '), write(StarlogCode), nl, nl,
    
    write('Converting Starlog to Prolog and storing in variable:'), nl,
    starlog_to_prolog_code(A is "x":"y", PrologCode),
    write('  PrologCode = '), write(PrologCode), nl.

% Demo 2: Manipulating the returned code
demo_code_manipulation :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 2: Code Manipulation - Using Returned Code'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Get multiple conversions and compare:'), nl,
    starlog_output_code(string_concat(A, B, C), Code1),
    starlog_output_code(append([1,2], [3,4], L), Code2),
    write('  Code1 = '), write(Code1), nl,
    write('  Code2 = '), write(Code2), nl, nl,
    
    write('Extracting parts of the code:'), nl,
    starlog_output_code(string_concat("test", "123", R), (Var is Expr)),
    write('  Variable: '), write(Var), nl,
    write('  Expression: '), write(Expr), nl.

% Demo 3: Comparison of /1 vs /2 versions
demo_comparison :-
    write('═══════════════════════════════════════════════════════════'), nl,
    write('Demo 3: Comparison - /1 prints, /2 returns without printing'), nl,
    write('═══════════════════════════════════════════════════════════'), nl, nl,
    
    write('Using /1 version (prints to stdout):'), nl,
    write('  Output: '),
    starlog_output_code(append([a], [b], L)),
    nl,
    
    write('Using /2 version (silent, returns in variable):'), nl,
    starlog_output_code(append([a], [b], L), Code),
    write('  Code = '), write(Code), nl, nl,
    
    write('Same for starlog_to_prolog_code:'), nl,
    write('  /1 version output: '),
    starlog_to_prolog_code(X is [1]&[2]),
    nl,
    write('  /2 version (silent):'), nl,
    starlog_to_prolog_code(X is [1]&[2], PCode),
    write('    PCode = '), write(PCode), nl.

:- initialization(main, main).
