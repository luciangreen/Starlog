% demo_algebra_solver.pl
% Demonstration of the algebraic equation solver

:- use_module(algebra_solver).

% Example 1: Problem statement equation
% (Y+5)/2 is 2
demo_problem_statement :-
    write('Example 1: Solving (Y+5)/2 = 2'), nl,
    solve_equation((Y+5)/2 is 2, Y, Solution),
    format('  Solution: Y = ~w~n', [Solution]), nl.

% Example 2: Simple linear equation
demo_simple_linear :-
    write('Example 2: Solving 3*X+5 = 20'), nl,
    solve_equation(3*X+5 is 20, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

% Example 3: Equation with division
demo_division :-
    write('Example 3: Solving (X-4)/2 = 3'), nl,
    solve_equation((X-4)/2 is 3, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

% Example 4: Quadratic equation (square)
demo_square :-
    write('Example 4: Solving X**2 = 25'), nl,
    solve_equation(X**2 is 25, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

% Example 5: Variable on the right side
demo_variable_right :-
    write('Example 5: Solving 10 = 2*X'), nl,
    solve_equation(10 is 2*X, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

% Example 6: Subtraction from constant
demo_subtraction :-
    write('Example 6: Solving 20-X = 8'), nl,
    solve_equation(20-X is 8, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

% Example 7: Complex nested equation
demo_complex :-
    write('Example 7: Solving ((X+2)*3-1)/2 = 7'), nl,
    solve_equation(((X+2)*3-1)/2 is 7, X, Solution),
    format('  Solution: X = ~w~n', [Solution]), nl.

run_demos :-
    write('======================================================'), nl,
    write('Algebra Solver Demonstration'), nl,
    write('Solving algebraic equations by applying operations'), nl,
    write('to both sides to isolate the variable'), nl,
    write('======================================================'), nl, nl,
    
    demo_problem_statement,
    demo_simple_linear,
    demo_division,
    demo_square,
    demo_variable_right,
    demo_subtraction,
    demo_complex,
    
    write('======================================================'), nl,
    write('All demonstrations complete!'), nl,
    write('======================================================'), nl.

:- initialization(run_demos, main).
