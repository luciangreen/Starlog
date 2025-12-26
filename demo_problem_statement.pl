% demo_problem_statement.pl
% Demonstration solving the specific equation from the problem statement

:- use_module(algebra_solver).

main :-
    write('======================================================'), nl,
    write('Algebra Solver - Problem Statement Demonstration'), nl,
    write('======================================================'), nl, nl,
    
    write('Problem: (Y+5)/2 is 2'), nl,
    write('Task: Apply operations to both sides to solve for Y'), nl, nl,
    
    write('Solution steps:'), nl,
    write('  Step 1: (Y+5)/2 = 2'), nl,
    write('  Step 2: Multiply both sides by 2: Y+5 = 4'), nl,
    write('  Step 3: Subtract 5 from both sides: Y = -1'), nl, nl,
    
    write('Using the algebra solver:'), nl,
    solve_equation((Y+5)/2 is 2, Y, Solution),
    format('  solve_equation((Y+5)/2 is 2, Y, Solution).~n'),
    format('  => Y = ~w~n', [Solution]), nl,
    
    write('Verification:'), nl,
    Verification is (Solution+5)/2,
    format('  Substitute Y = ~w into (Y+5)/2:~n', [Solution]),
    format('  (~w+5)/2 = ~w ✓~n', [Solution, Verification]), nl,
    
    write('======================================================'), nl,
    write('Problem solved successfully!'), nl,
    write('======================================================'), nl.

:- initialization(main, main).
