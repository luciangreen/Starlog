% Find the correct matrix for solution (1, 5, 1)
% We need to find coefficients a,b,c,d such that:
% a*1 + b*5 + c*1 = d

test_solution :-
    write('Testing if (1,5,1) is a solution to the given matrix:'), nl,
    X1 = 1, X2 = 5, X3 = 1,
    
    % Row 1: 2x1 + 1x2 + 3x3 = ?
    R1 is 2*X1 + 1*X2 + 3*X3,
    format('Row 1: 2*~w + 1*~w + 3*~w = ~w (expected 2)~n', [X1, X2, X3, R1]),
    
    % Row 2: 3x1 + 0x2 + 1.5x3 = ?
    R2 is 3*X1 + 0*X2 + 1.5*X3,
    format('Row 2: 3*~w + 0*~w + 1.5*~w = ~w (expected 1)~n', [X1, X2, X3, R2]),
    
    % Row 3: 0x1 + 0x2 + 1x3 = ?
    R3 is 0*X1 + 0*X2 + 1*X3,
    format('Row 3: 0*~w + 0*~w + 1*~w = ~w (expected 1)~n', [X1, X2, X3, R3]),
    nl,
    
    % Try to find the correct right-hand side values
    write('Correct matrix would be:'), nl,
    format('  2   1   3   ~w~n', [R1]),
    format('  3   0   1.5 ~w~n', [R2]),
    format('  0   0   1   ~w~n', [R3]).

:- initialization(test_solution, main).
