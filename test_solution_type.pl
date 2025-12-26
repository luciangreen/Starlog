:- use_module(gaussian_elimination).

test :-
    Matrix = [[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]],
    gaussian_elimination(Matrix, Reduced, SolutionType),
    write('Reduced matrix:'), nl,
    forall(member(Row, Reduced), (write('  '), write(Row), nl)),
    format('Solution type: ~w~n', [SolutionType]),
    
    % Count rows with non-zero coefficients
    length(Matrix, NumRows),
    (Matrix = [FirstRow|_] -> length(FirstRow, NumCols) ; NumCols = 0),
    NumVars is NumCols - 1,
    format('NumVars: ~w~n', [NumVars]),
    
    findall(R, (member(R, Reduced), has_non_zero_coeff(R, NumVars)), NonZeroRows),
    length(NonZeroRows, Count),
    format('Non-zero rows: ~w~n', [NonZeroRows]),
    format('Count: ~w~n', [Count]).

has_non_zero_coeff(Row, NumVars) :-
    first_n(Row, NumVars, Coeffs),
    member(Coeff, Coeffs),
    abs(Coeff) > 1e-10.

first_n(_, 0, []) :- !.
first_n([H|T], N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    first_n(T, N1, R).

:- initialization(test, main).
