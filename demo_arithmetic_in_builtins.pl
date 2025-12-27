% demo_arithmetic_in_builtins.pl
% Demonstration of arithmetic expressions in builtin function arguments
% This showcases the completed requirement: A is number_string(2^2):2

:- use_module(starlog).

demo_problem_statement :-
    write('===================================='), nl,
    write('Problem Statement Example'), nl,
    write('===================================='), nl,
    write('Expression: A is number_string(2^2):2'), nl, nl,
    
    A is number_string(2^2):2,
    
    write('How it works:'), nl,
    write('  1. 2^2 evaluates to 4'), nl,
    write('  2. number_string(4) converts to "4"'), nl,
    write('  3. "4" : 2 concatenates to "42"'), nl, nl,
    
    write('Result:'), nl,
    format('  A = ~q~n', [A]), nl.

demo_basic_arithmetic :-
    write('===================================='), nl,
    write('Basic Arithmetic in Builtins'), nl,
    write('===================================='), nl,
    
    write('Example 1: number_string(10+5)'), nl,
    A is number_string(10+5),
    format('  Result: ~q~n', [A]), nl,
    
    write('Example 2: number_string(3*7)'), nl,
    B is number_string(3*7),
    format('  Result: ~q~n', [B]), nl,
    
    write('Example 3: number_string(100/4)'), nl,
    C is number_string(100/4),
    format('  Result: ~q~n', [C]), nl.

demo_nested_arithmetic :-
    write('===================================='), nl,
    write('Nested Arithmetic Operations'), nl,
    write('===================================='), nl,
    
    write('Example 1: string_length(number_string(10^2))'), nl,
    A is string_length(number_string(10^2)),
    format('  10^2 = 100, string "100" has length: ~w~n', [A]), nl,
    
    write('Example 2: reverse([1,2,3]) with arithmetic elsewhere'), nl,
    B is reverse([1,2,3]),
    format('  Result: ~w~n', [B]), nl.

demo_complex_concatenation :-
    write('===================================='), nl,
    write('Complex Concatenation Examples'), nl,
    write('===================================='), nl,
    
    write('Example 1: number_string(2*3) : number_string(4+5)'), nl,
    A is number_string(2*3) : number_string(4+5),
    format('  "6" : "9" = ~q~n', [A]), nl,
    
    write('Example 2: "Result: " : number_string(7^2)'), nl,
    B is "Result: " : number_string(7^2),
    format('  ~q~n', [B]), nl,
    
    write('Example 3: number_string(1+1) : "-" : number_string(2*2)'), nl,
    C is number_string(1+1) : "-" : number_string(2*2),
    format('  ~q~n', [C]), nl.

demo_practical_examples :-
    write('===================================='), nl,
    write('Practical Examples'), nl,
    write('===================================='), nl,
    
    write('Generating formatted output:'), nl,
    X is 5,
    Y is 10,
    Sum is X + Y,
    Message is "The sum of " : number_string(X) : " and " : number_string(Y) : " is " : number_string(Sum),
    format('  ~q~n', [Message]), nl,
    
    write('Creating numerical sequences:'), nl,
    Seq1 is number_string(1*10),
    Seq2 is number_string(2*10),
    Seq3 is number_string(3*10),
    Sequence is Seq1 : "," : Seq2 : "," : Seq3,
    format('  Sequence: ~q~n', [Sequence]), nl.

main :-
    write(''), nl,
    write('╔════════════════════════════════════════════════════════╗'), nl,
    write('║  Arithmetic in Builtin Arguments - Demonstration      ║'), nl,
    write('║  Feature: A is number_string(2^2):2                   ║'), nl,
    write('╚════════════════════════════════════════════════════════╝'), nl, nl,
    
    demo_problem_statement,
    demo_basic_arithmetic,
    demo_nested_arithmetic,
    demo_complex_concatenation,
    demo_practical_examples,
    
    write('===================================='), nl,
    write('All demonstrations completed! ✓'), nl,
    write('===================================='), nl.

:- initialization(main, main).
