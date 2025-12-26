:- use_module(gaussian_elimination).

test1 :-
    write('Testing matrix from problem statement:'), nl,
    Matrix = [[2, 1, 3, 2], [3, 0, 1.5, 1], [0, 0, 1, 1]],
    write('Original matrix:'), nl,
    forall(member(Row, Matrix), (write('  '), write(Row), nl)),
    nl,
    gaussian_elimination(Matrix, ReducedMatrix, SolutionType),
    write('Reduced matrix:'), nl,
    forall(member(Row, ReducedMatrix), (write('  '), write(Row), nl)),
    format('Solution type: ~w~n', [SolutionType]),
    nl,
    solve_system(Matrix, Solution, ST),
    format('Solution type: ~w~n', [ST]),
    format('Solution: ~w~n', [Solution]).

:- initialization(test1, main).
