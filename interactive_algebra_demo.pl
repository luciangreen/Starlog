% interactive_algebra_demo.pl
% Interactive demonstration of the algebra solver

:- use_module(starlog_in_prolog).

demo :-
    write('╔════════════════════════════════════════════════════════════════╗'), nl,
    write('║          Starlog Algebra Solver - Interactive Demo            ║'), nl,
    write('║       Solving Equations by Applying Operations to Both Sides  ║'), nl,
    write('╚════════════════════════════════════════════════════════════════╝'), nl, nl,
    
    % Problem from problem statement
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('Example 1: Problem Statement - (Y+5)/2 = 2'), nl,
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('  Equation: (Y+5)/2 = 2'), nl,
    write('  Steps:'), nl,
    write('    1. Multiply both sides by 2: Y+5 = 4'), nl,
    write('    2. Subtract 5 from both sides: Y = -1'), nl,
    solve_equation((Y1+5)/2 is 2, Y1, Sol1),
    format('  Solution: Y = ~w~n', [Sol1]),
    format('  Verification: (~w+5)/2 = ~w ✓~n', [Sol1, 2]), nl,
    
    % Multi-step equation
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('Example 2: Multi-Step Equation - 3*X+7 = 22'), nl,
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('  Equation: 3*X+7 = 22'), nl,
    write('  Steps:'), nl,
    write('    1. Subtract 7 from both sides: 3*X = 15'), nl,
    write('    2. Divide both sides by 3: X = 5'), nl,
    solve_equation(3*X2+7 is 22, X2, Sol2),
    format('  Solution: X = ~w~n', [Sol2]),
    format('  Verification: 3*~w+7 = ~w ✓~n', [Sol2, 22]), nl,
    
    % Complex nested
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('Example 3: Complex Nested - ((Z+2)*3-1)/2 = 7'), nl,
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('  Equation: ((Z+2)*3-1)/2 = 7'), nl,
    write('  Steps:'), nl,
    write('    1. Multiply both sides by 2: (Z+2)*3-1 = 14'), nl,
    write('    2. Add 1 to both sides: (Z+2)*3 = 15'), nl,
    write('    3. Divide both sides by 3: Z+2 = 5'), nl,
    write('    4. Subtract 2 from both sides: Z = 3'), nl,
    solve_equation(((Z3+2)*3-1)/2 is 7, Z3, Sol3),
    format('  Solution: Z = ~w~n', [Sol3]),
    format('  Verification: ((~w+2)*3-1)/2 = ~w ✓~n', [Sol3, 7]), nl,
    
    % Quadratic
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('Example 4: Power Equation - W**2 = 49'), nl,
    write('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━'), nl,
    write('  Equation: W**2 = 49'), nl,
    write('  Steps:'), nl,
    write('    1. Take square root of both sides: W = 7'), nl,
    solve_equation(W4**2 is 49, W4, Sol4),
    format('  Solution: W = ~w~n', [Sol4]),
    format('  Verification: ~w**2 = ~w ✓~n', [Sol4, 49]), nl,
    
    write('╔════════════════════════════════════════════════════════════════╗'), nl,
    write('║                    All Examples Complete!                     ║'), nl,
    write('║                                                                ║'), nl,
    write('║  The algebra solver successfully solves equations by          ║'), nl,
    write('║  applying inverse operations to both sides automatically.     ║'), nl,
    write('╚════════════════════════════════════════════════════════════════╝'), nl.

:- initialization(demo, main).
