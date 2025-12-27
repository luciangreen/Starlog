% demo_problem_statement_complete.pl
% Demonstration of the completed problem statement requirements:
% "Complete a:A is B:b and a•A is B•b"

:- use_module(starlog).

% Helper to ensure a value is a string
ensure_string(Val, Str) :-
    (atom(Val) -> atom_string(Val, Str) ; Str = Val).

demo_string_concat :-
    write('===================================='), nl,
    write('String Concatenation Dual Expression'), nl,
    write('===================================='), nl,
    write('Problem: (a : A) is (B : b)'), nl,
    write('Meaning: a + A = B + b (where + is string concat)'), nl, nl,
    
    (a : A) is (B : b),
    
    write('Solution:'), nl,
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]), nl,
    
    write('Verification:'), nl,
    % Convert to strings for string_concat verification
    ensure_string(a, AS1),
    ensure_string(A, AS2),
    string_concat(AS1, AS2, LHS),
    ensure_string(B, BS1),
    ensure_string(b, BS2),
    string_concat(BS1, BS2, RHS),
    format('  a + ~w = ~w~n', [A, LHS]),
    format('  ~w + b = ~w~n', [B, RHS]),
    (LHS = RHS -> write('  ✓ Both sides equal!') ; write('  ✗ Error')), nl, nl.

demo_atom_concat :-
    write('===================================='), nl,
    write('Atom Concatenation Dual Expression'), nl,
    write('===================================='), nl,
    write('Problem: (a • A) is (B • b)'), nl,
    write('Meaning: a • A = B • b (where • is atom concat)'), nl, nl,
    
    (a • A) is (B • b),
    
    write('Solution:'), nl,
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]), nl,
    
    write('Verification:'), nl,
    atom_concat(a, A, LHS),
    atom_concat(B, b, RHS),
    format('  a • ~w = ~w~n', [A, LHS]),
    format('  ~w • b = ~w~n', [B, RHS]),
    (LHS = RHS -> write('  ✓ Both sides equal!') ; write('  ✗ Error')), nl, nl.

demo_more_examples :-
    write('===================================='), nl,
    write('Additional Examples'), nl,
    write('===================================='), nl, nl,
    
    write('Example 1: (x : Y) is (Z : y)'), nl,
    (x : Y1) is (Z1 : y),
    format('  Y = ~w, Z = ~w~n', [Y1, Z1]), nl,
    
    write('Example 2: (hello • World) is (Greeting • world)'), nl,
    (hello • World) is (Greeting • world),
    format('  World = ~w, Greeting = ~w~n', [World, Greeting]), nl,
    
    write('Example 3: List dual expression ([1,2] & A) is (B & [3])'), nl,
    ([1,2] & A2) is (B2 & [3]),
    format('  A = ~w, B = ~w~n', [A2, B2]), nl,
    
    write('Example 4: Triple string concat (a : A : c) is (B : b : c)'), nl,
    (a : A3 : c) is (B3 : b : c),
    format('  A = ~w, B = ~w~n', [A3, B3]), nl,
    
    write('Example 5: Triple atom concat (a • A • c) is (B • b • c)'), nl,
    (a • A4 • c) is (B4 • b • c),
    format('  A = ~w, B = ~w~n', [A4, B4]), nl,
    
    write('Example 6: Quadruple concat (a : A : c : d) is (B : b : c : d)'), nl,
    (a : A5 : c : d) is (B5 : b : c : d),
    format('  A = ~w, B = ~w~n', [A5, B5]), nl.

main :-
    write(''), nl,
    write('╔════════════════════════════════════════════════════════╗'), nl,
    write('║  Problem Statement Completion Demonstration           ║'), nl,
    write('║  Complete a:A:c is B:b:c and a•A•c is B•b•c           ║'), nl,
    write('║  (or any atom, string, list or variable in any        ║'), nl,
    write('║   position of any configuration)                      ║'), nl,
    write('╚════════════════════════════════════════════════════════╝'), nl, nl,
    
    demo_string_concat,
    demo_atom_concat,
    demo_more_examples,
    
    write('===================================='), nl,
    write('All demonstrations completed! ✓'), nl,
    write('===================================='), nl.

:- initialization(main, main).
