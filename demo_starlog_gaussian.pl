% demo_starlog_gaussian.pl
% Demonstration of Gaussian elimination using Starlog syntax

:- use_module(starlog).

% Example 1: Using Starlog syntax to solve a system
demo_starlog_syntax :-
    write('Gaussian Elimination with Starlog Syntax'), nl,
    write('==========================================='), nl, nl,
    
    write('Example: Solve the system'), nl,
    write('  2x + y = 5'), nl,
    write('  x - y = 1'), nl, nl,
    
    % Using Starlog "is" syntax
    Matrix = [[2, 1, 5], [1, -1, 1]],
    write('Using Starlog syntax: Solution is solve_system(Matrix)'), nl,
    
    % This uses the Starlog expansion
    starlog_call(Solution is solve_system(Matrix)),
    
    format('Solution: ~w~n', [Solution]),
    Solution = [X, Y],
    format('  x = ~w~n', [X]),
    format('  y = ~w~n', [Y]),
    nl.

% Example 2: Solving multiple systems
demo_multiple_systems :-
    write('Solving multiple systems:'), nl,
    write('-------------------------'), nl, nl,
    
    % System 1
    write('System 1: 3x3 matrix'), nl,
    Matrix1 = [[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]],
    starlog_call(Sol1 is solve_system(Matrix1)),
    format('  Solution: ~w~n', [Sol1]),
    nl,
    
    % System 2
    write('System 2: 2x2 matrix'), nl,
    Matrix2 = [[1, 1, 3], [2, -1, 1]],
    starlog_call(Sol2 is solve_system(Matrix2)),
    format('  Solution: ~w~n', [Sol2]),
    nl.

% Example 3: Getting solution type
demo_solution_types :-
    write('Detecting solution types:'), nl,
    write('-------------------------'), nl, nl,
    
    % Unique solution
    write('1. Unique solution:'), nl,
    M1 = [[2, 1, 5], [1, -1, 1]],
    solve_system(M1, S1, T1),
    format('   Matrix: ~w~n', [M1]),
    format('   Type: ~w~n', [T1]),
    format('   Solution: ~w~n', [S1]),
    nl,
    
    % Infinite solutions
    write('2. Infinite solutions (underdetermined):'), nl,
    M2 = [[1, 1, 1, 6], [2, 1, 3, 14]],
    solve_system(M2, S2, T2),
    format('   Matrix: ~w~n', [M2]),
    format('   Type: ~w~n', [T2]),
    format('   Solution: ~w~n', [S2]),
    nl.

run_demos :-
    write('===================================================='), nl,
    write('  Gaussian Elimination with Starlog Syntax'), nl,
    write('===================================================='), nl, nl,
    
    demo_starlog_syntax,
    demo_multiple_systems,
    demo_solution_types,
    
    write('===================================================='), nl,
    write('  All demonstrations complete!'), nl,
    write('===================================================='), nl.

:- initialization(run_demos, main).
