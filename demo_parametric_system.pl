% demo_parametric_system.pl
% Demonstration of solving a parametric system with infinite solutions
% Problem: Solve the system where x2 is a free parameter

:- use_module(gaussian_elimination).
:- use_module(starlog_in_prolog).

% The system from the problem statement:
% 2x₁ + 0x₂ + 3x₃ = 2
% 3x₁ + 0x₂ + 0x₃ = 1
% 0x₁ + 0x₂ + 2.25x₃ = 1
%
% Expected solution: (x₁, x₂, x₃) = (1/3, t, 4/9)
% where t is a free parameter (x₂ can be any value)

demo_parametric_system :-
    write('======================================================'), nl,
    write('Parametric System with Infinite Solutions'), nl,
    write('======================================================'), nl, nl,
    
    write('System of equations:'), nl,
    write('  2x₁ + 0x₂ + 3x₃ = 2'), nl,
    write('  3x₁ + 0x₂ + 0x₃ = 1'), nl,
    write('  0x₁ + 0x₂ + 2.25x₃ = 1'), nl, nl,
    
    write('Augmented matrix:'), nl,
    Matrix = [[2, 0, 3, 2], [3, 0, 0, 1], [0, 0, 2.25, 1]],
    format('  ~w~n', [Matrix]), nl,
    
    write('Solving using Gaussian elimination...'), nl,
    solve_system(Matrix, Solution, SolutionType),
    
    format('Solution type: ~w~n', [SolutionType]), nl,
    
    (SolutionType = infinite ->
        write('The system has infinite solutions!'), nl,
        write('x₂ is a free parameter (can take any value t)'), nl, nl,
        
        format('Particular solution: ~w~n', [Solution]),
        (Solution = [X1, X2, X3] ->
            format('  x₁ = ~w~n', [X1]),
            format('  x₂ = ~w (free parameter t)~n', [X2]),
            format('  x₃ = ~w~n', [X3]), nl
        ;
            true
        ),
        
        % Calculate expected values
        ExpectedX1 is 1/3,
        ExpectedX3 is 4/9,
        
        write('Expected solution: (x₁, x₂, x₃) = (1/3, t, 4/9)'), nl,
        format('  x₁ = 1/3 ≈ ~w~n', [ExpectedX1]),
        write('  x₂ = t (free parameter)'), nl,
        format('  x₃ = 4/9 ≈ ~w~n', [ExpectedX3]), nl, nl,
        
        % Verify the particular solution
        write('Verification with the particular solution:'), nl,
        (Solution = [X1Val, X2Val, X3Val] ->
            % For free variable, use 0 for verification (it works for any value)
            (var(X2Val) -> X2Check = 0 ; X2Check = X2Val),
            Check1 is 2*X1Val + 0*X2Check + 3*X3Val,
            Check2 is 3*X1Val + 0*X2Check + 0*X3Val,
            Check3 is 0*X1Val + 0*X2Check + 2.25*X3Val,
            format('  Equation 1: 2(~w) + 0(t) + 3(~w) = ~w (should be 2)~n', 
                   [X1Val, X3Val, Check1]),
            format('  Equation 2: 3(~w) + 0(t) + 0(~w) = ~w (should be 1)~n', 
                   [X1Val, X3Val, Check2]),
            format('  Equation 3: 0(~w) + 0(t) + 2.25(~w) = ~w (should be 1)~n', 
                   [X1Val, X3Val, Check3]),
            
            (abs(Check1 - 2) < 0.001, abs(Check2 - 1) < 0.001, abs(Check3 - 1) < 0.001 ->
                nl, write('✓ Solution verified correctly!'), nl,
                write('  (Note: x₂ = t can be any value, we used 0 for verification)'), nl
            ;
                nl, write('✗ Solution verification failed!'), nl
            )
        ;
            write('Could not extract solution values'), nl
        )
    ;
        format('Expected infinite solutions but got: ~w~n', [SolutionType])
    ),
    
    nl,
    write('======================================================'), nl,
    write('General solution:'), nl,
    write('  For any value of t, the solution is:'), nl,
    write('    x₁ = 1/3'), nl,
    write('    x₂ = t  (free parameter)'), nl,
    write('    x₃ = 4/9'), nl,
    write('======================================================'), nl.

:- initialization(demo_parametric_system, main).
