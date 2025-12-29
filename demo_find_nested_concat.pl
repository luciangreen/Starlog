% demo_find_nested_concat.pl
% Demonstration of find/3 with nested concatenation expressions
% This demonstrates the solution to: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)

:- use_module(starlog).

demo_find_nested_concat :-
    write('==================================================================='), nl,
    write('Demonstration of find/3 with Nested Concatenation'), nl,
    write('Problem: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)'), nl,
    write('==================================================================='), nl, nl,
    
    % Example 1: The exact problem statement
    write('Example 1: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)'), nl,
    write('  Finds A and C such that A:"a":C equals "a":"a":"c"'), nl,
    find([A1,C1], starlog_call([A1:a:C1] is [a:a:c]), Result1),
    format('  Result: ~w~n', [Result1]),
    format('  This means: A="~w", C="~w" such that "~w":"a":"~w" = "aac"~n', [A1, C1, A1, C1]),
    nl,
    
    % Example 2: Different values
    write('Example 2: find([A,C], starlog_call([A:"_":C] is ["hello":"_":"world"]), Result)'), nl,
    write('  Finds A and C such that A:"_":C equals "hello":"_":"world"'), nl,
    find([A2,C2], starlog_call([A2:"_":C2] is ["hello":"_":"world"]), Result2),
    format('  Result: ~w~n', [Result2]),
    format('  This means: A="~w", C="~w"~n', [A2, C2]),
    nl,
    
    % Example 3: Four-way concatenation
    write('Example 3: find([A,C], starlog_call([A:"|":C:"|"] is ["x":"|":"y":"|"]), Result)'), nl,
    write('  Finds A and C in a four-part pattern'), nl,
    find([A3,C3], starlog_call([A3:"|":C3:"|"] is ["x":"|":"y":"|"]), Result3),
    format('  Result: ~w~n', [Result3]),
    format('  This means: A="~w", C="~w"~n', [A3, C3]),
    nl,
    
    % Example 4: Five-way concatenation
    write('Example 4: find([A,C,E], starlog_call([A:"-":C:"-":E] is ["a":"-":"b":"-":"c"]), Result)'), nl,
    write('  Finds A, C, and E in a five-part pattern'), nl,
    find([A4,C4,E4], starlog_call([A4:"-":C4:"-":E4] is ["a":"-":"b":"-":"c"]), Result4),
    format('  Result: ~w~n', [Result4]),
    format('  This means: A="~w", C="~w", E="~w"~n', [A4, C4, E4]),
    nl,
    
    % Example 5: Atom concatenation
    write('Example 5: find([A,C], starlog_call([A•x•C] is [y•x•z]), Result)'), nl,
    write('  Same pattern but with atom concatenation (•)'), nl,
    find([A5,C5], starlog_call([A5•x•C5] is [y•x•z]), Result5),
    format('  Result: ~w~n', [Result5]),
    format('  This means: A=~w, C=~w (atoms)~n', [A5, C5]),
    nl,
    
    % Example 6: Direct dual expression (without find)
    write('Example 6: starlog_call((A:a:C) is (b:a:c))'), nl,
    write('  Direct use without find (binds A and C in current scope)'), nl,
    starlog_call((A6:a:C6) is (b:a:c)),
    format('  A = ~w, C = ~w~n', [A6, C6]),
    nl,
    
    % Example 7: Reverse pattern
    write('Example 7: starlog_call([a:b:c] is [A:b:C])'), nl,
    write('  Variables on the right side'), nl,
    starlog_call([a:b:c] is [A7:b:C7]),
    format('  A = ~w, C = ~w~n', [A7, C7]),
    nl,
    
    write('==================================================================='), nl,
    write('Key Points:'), nl,
    write('- The pattern [A:a:C] is [a:a:c] solves for multiple variables'), nl,
    write('- Works with any number of concatenations (3-way, 4-way, 5-way, etc.)'), nl,
    write('- Supports both string (:) and atom (•) concatenation'), nl,
    write('- Can have variables on either or both sides'), nl,
    write('- find/3 captures the solution in a list'), nl,
    write('==================================================================='), nl.

:- initialization(demo_find_nested_concat, main).
