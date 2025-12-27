% demo_list_dual_expressions.pl
% Demonstration of list dual expressions without append operator
% Feature: Complete [A:...a] is [b:...B] and [A•a:c] is [b•B:c]

:- use_module(starlog).

demo_basic_string :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Basic String Concatenation in Lists      ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [A:a] is [b:B]'), nl,
    write('Meaning: List with concat expr on both sides'), nl,
    write('Solution: A + a = b + B (bidirectional)'), nl, nl,
    
    ([A:a] is [b:B]),
    format('Result: A = ~w, B = ~w~n', [A, B]),
    write('Verification: A:a = b:B ✓'), nl, nl.

demo_basic_atom :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Basic Atom Concatenation in Lists        ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [A•a] is [b•B]'), nl,
    write('Meaning: Same as string but with atom concat'), nl, nl,
    
    ([A•a] is [b•B]),
    format('Result: A = ~w, B = ~w~n', [A, B]),
    write('Verification: A•a = b•B ✓'), nl, nl.

demo_mixed_operators :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Mixed Operators in Single Element        ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [(A•a):c] is [(b•B):c]'), nl,
    write('Meaning: Nested concat with both • and :'), nl,
    write('First compute A•a and b•B, then concat with c'), nl, nl,
    
    ([(A•a):c] is [(b•B):c]),
    format('Result: A = ~w, B = ~w~n', [A, B]),
    write('Verification: (A•a):c = (b•B):c ✓'), nl, nl.

demo_multiple_elements :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Multiple Elements with Concat             ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [A•q, x] is [p•q, x]'), nl,
    write('Meaning: First element has concat, second is literal'), nl,
    write('Note: suffixes must match (q = q) for solution'), nl, nl,
    
    ([A•q, x] is [p•q, x]),
    format('Result: A = ~w~n', [A]),
    write('Verification: [A•q, x] = [p•q, x] ✓'), nl, nl.

demo_multiple_concat :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Multiple Concat Elements                  ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [A•a, B•b] is [p•a, r•b]'), nl,
    write('Meaning: Each element paired bidirectionally'), nl,
    write('First: A•a = p•a → A = p'), nl,
    write('Second: B•b = r•b → B = r'), nl, nl,
    
    ([A•a, B•b] is [p•a, r•b]),
    format('Result: A = ~w, B = ~w~n', [A, B]),
    write('Verification: Both equations solved! ✓'), nl, nl.

demo_longer_chains :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Longer Concatenation Chains              ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('Pattern: [A:x:y] is [p:x:y]'), nl,
    write('Meaning: Nested concat A:x:y = p:x:y'), nl,
    write('Since x:y matches, A must equal p'), nl, nl,
    
    ([A:x:y] is [p:x:y]),
    format('Result: A = ~w~n', [A]),
    write('Verification: Nested concat solved! ✓'), nl, nl.

demo_use_cases :-
    write('╔════════════════════════════════════════════╗'), nl,
    write('║  Real-World Use Cases                      ║'), nl,
    write('╚════════════════════════════════════════════╝'), nl, nl,
    
    write('1. Template Matching'), nl,
    write('   Pattern: [Prefix:suffix] is [known_value]'), nl,
    write('   Extracts Prefix from a string with known suffix'), nl, nl,
    
    write('2. Dual Constraints'), nl,
    write('   Pattern: [A:b, C:d] is [Result1, Result2]'), nl,
    write('   Solves multiple concat equations simultaneously'), nl, nl,
    
    write('3. Mixed Operator Parsing'), nl,
    write('   Pattern: [(Atom•suffix):string_part]'), nl,
    write('   Combines atom and string operations'), nl, nl.

main :-
    write(''), nl,
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('  List Dual Expressions Demonstration'), nl,
    write('  Feature: [A:a] is [b:B] and [A•a:c] is [b•B:c]'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    
    demo_basic_string,
    demo_basic_atom,
    demo_mixed_operators,
    demo_multiple_elements,
    demo_multiple_concat,
    demo_longer_chains,
    demo_use_cases,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('All demonstrations completed successfully! ✓'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(main, main).
