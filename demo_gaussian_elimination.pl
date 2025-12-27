% demo_gaussian_elimination.pl
% Demonstration of Gaussian elimination solver

:- use_module(gaussian_elimination).
:- use_module(starlog).

% Example from problem statement:
% Gaussian elimination
% To get solution (x_1,x_2,x_3)=(1,5,1), the matrix should be:
% 2   1   3   10
% 3   0   1.5 4.5
% 0   0   1   1

demo_problem_statement_1 :-
    write('Example 1: System from problem statement (corrected)'), nl,
    write('Matrix (for solution x1=1, x2=5, x3=1):'), nl,
    write('  2   1   3   10'), nl,
    write('  3   0   1.5 4.5'), nl,
    write('  0   0   1   1'), nl,
    nl,
    Matrix = [[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    format('Solution type: ~w~n', [SolutionType]),
    format('Solution: ~w~n', [Solution]),
    (Solution = [X1, X2, X3] ->
        format('  x1 = ~w, x2 = ~w, x3 = ~w~n', [X1, X2, X3])
    ;
        true
    ),
    nl.

% Example as literally stated in problem (with values 2, 1, 1)
demo_literal_problem :-
    write('Example 1b: System as literally stated in problem'), nl,
    write('Matrix:'), nl,
    write('  2   1   3   2'), nl,
    write('  3   0   1.5 1'), nl,
    write('  0   0   1   1'), nl,
    nl,
    Matrix = [[2, 1, 3, 2], [3, 0, 1.5, 1], [0, 0, 1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    format('Solution type: ~w~n', [SolutionType]),
    format('Solution: ~w~n', [Solution]),
    (Solution = [X1, X2, X3] ->
        format('  x1 = ~w, x2 = ~w, x3 = ~w~n', [X1, X2, X3])
    ;
        true
    ),
    nl.

% Example with infinite solutions (underdetermined system)
% This system has infinitely many solutions
demo_infinite_solutions :-
    write('Example 2: Underdetermined system (2 equations, 3 unknowns)'), nl,
    write('Matrix:'), nl,
    write('  1   1   1   6'), nl,
    write('  2   1   3   14'), nl,
    nl,
    Matrix = [[1, 1, 1, 6], [2, 1, 3, 14]],
    solve_system(Matrix, Solution, SolutionType),
    format('Solution type: ~w~n', [SolutionType]),
    format('Solution (one particular solution): ~w~n', [Solution]),
    write('Note: With infinite solutions, there are free variables.'), nl,
    nl.

% Simple 2x2 system
demo_simple_2x2 :-
    write('Example 3: Simple 2x2 system'), nl,
    write('  2x + y = 5'), nl,
    write('  x - y = 1'), nl,
    write('Matrix:'), nl,
    write('  2   1   5'), nl,
    write('  1  -1   1'), nl,
    nl,
    Matrix = [[2, 1, 5], [1, -1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    format('Solution type: ~w~n', [SolutionType]),
    format('Solution: ~w~n', [Solution]),
    (Solution = [X, Y] ->
        format('  x = ~w, y = ~w~n', [X, Y])
    ;
        true
    ),
    nl.

% 3x3 system with unique solution
demo_3x3_unique :-
    write('Example 4: 3x3 system with unique solution'), nl,
    write('  x + 2y + 3z = 14'), nl,
    write('  2x + y + z = 9'), nl,
    write('  3x + 2y + z = 13'), nl,
    write('Matrix:'), nl,
    write('  1   2   3   14'), nl,
    write('  2   1   1    9'), nl,
    write('  3   2   1   13'), nl,
    nl,
    Matrix = [[1, 2, 3, 14], [2, 1, 1, 9], [3, 2, 1, 13]],
    solve_system(Matrix, Solution, SolutionType),
    format('Solution type: ~w~n', [SolutionType]),
    format('Solution: ~w~n', [Solution]),
    (Solution = [X, Y, Z] ->
        format('  x = ~w, y = ~w, z = ~w~n', [X, Y, Z])
    ;
        true
    ),
    nl.

% Example using Starlog syntax
demo_starlog_syntax :-
    write('Example 5: Using Starlog syntax'), nl,
    write('Solve system [[2, 1, 5], [1, -1, 1]]'), nl,
    nl,
    % Demonstrate that we can use the solver with Starlog syntax
    Matrix = [[2, 1, 5], [1, -1, 1]],
    Solution is solve_system(Matrix),
    format('Solution: ~w~n', [Solution]),
    nl.

run_demos :-
    write('======================================================'), nl,
    write('Gaussian Elimination Solver Demonstration'), nl,
    write('Solving systems of linear equations'), nl,
    write('======================================================'), nl, nl,
    
    catch(demo_problem_statement_1, E1, (write('Error in example 1: '), write(E1), nl, nl)),
    catch(demo_literal_problem, E1b, (write('Error in example 1b: '), write(E1b), nl, nl)),
    catch(demo_infinite_solutions, E2, (write('Error in example 2: '), write(E2), nl, nl)),
    catch(demo_simple_2x2, E3, (write('Error in example 3: '), write(E3), nl, nl)),
    catch(demo_3x3_unique, E4, (write('Error in example 4: '), write(E4), nl, nl)),
    
    write('======================================================'), nl,
    write('All demonstrations complete!'), nl,
    write('======================================================'), nl.

:- initialization(run_demos, main).
