% demo_peel_brackets.pl
% Demonstration of the peel_off_brackets feature
% Shows how to convert ["543"] to [5:(2+2):A]

:- use_module(starlog).

main :-
    write('══════════════════════════════════════════════════════'), nl,
    write('  Peel Off Nested Brackets - Demonstration'), nl,
    write('══════════════════════════════════════════════════════'), nl, nl,
    
    write('Problem: Peel off nested brackets from ["543"] is [5:(2+2):A]'), nl, nl,
    
    write('Demonstration:'), nl,
    write('  Input:  ["543"]'), nl,
    peel_off_brackets(["543"], [Result]),
    format('  Output: [~w]~n', [Result]), nl,
    
    write('Explanation:'), nl,
    write('  The string "543" is converted to:'), nl,
    write('    - Character \'5\' becomes 5'), nl,
    write('    - Character \'4\' becomes (2+2)'), nl,
    write('    - Character \'3\' becomes a variable A'), nl,
    write('  These are joined with the : operator (string concatenation)'), nl, nl,
    
    write('Additional Examples:'), nl,
    write('────────────────────────────────────────────────────'), nl, nl,
    
    write('Example 1: Single character'), nl,
    write('  Input:  ["5"]'), nl,
    peel_off_brackets(["5"], [R1]),
    format('  Output: [~w]~n~n', [R1]),
    
    write('Example 2: Two characters'), nl,
    write('  Input:  ["54"]'), nl,
    peel_off_brackets(["54"], [R2]),
    format('  Output: [~w]~n~n', [R2]),
    
    write('Example 3: With atom input'), nl,
    write('  Input:  [\'543\']'), nl,
    peel_off_brackets(['543'], [R3]),
    format('  Output: [~w]~n~n', [R3]),
    
    write('══════════════════════════════════════════════════════'), nl,
    write('  Demonstration Complete ✓'), nl,
    write('══════════════════════════════════════════════════════'), nl.

:- initialization(main, main).
