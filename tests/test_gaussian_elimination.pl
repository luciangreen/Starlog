% tests/test_gaussian_elimination.pl
% Tests for Gaussian elimination solver

:- use_module('../gaussian_elimination').

% Test 1: System with unique solution (1, 5, 1)
test_unique_solution_1 :-
    write('Test 1: Unique solution (1, 5, 1)'), nl,
    Matrix = [[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    SolutionType = unique,
    Solution = [X1, X2, X3],
    abs(X1 - 1.0) < 0.001,
    abs(X2 - 5.0) < 0.001,
    abs(X3 - 1.0) < 0.001,
    write('  PASSED'), nl, !.

test_unique_solution_1 :-
    write('  FAILED'), nl, fail.

% Test 2: Simple 2x2 system
test_2x2_system :-
    write('Test 2: 2x2 system'), nl,
    Matrix = [[2, 1, 5], [1, -1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    SolutionType = unique,
    Solution = [X, Y],
    abs(X - 2.0) < 0.001,
    abs(Y - 1.0) < 0.001,
    write('  PASSED'), nl, !.

test_2x2_system :-
    write('  FAILED'), nl, fail.

% Test 3: 3x3 system
test_3x3_system :-
    write('Test 3: 3x3 system'), nl,
    Matrix = [[1, 2, 3, 14], [2, 1, 1, 9], [3, 2, 1, 13]],
    solve_system(Matrix, Solution, SolutionType),
    SolutionType = unique,
    Solution = [X, Y, Z],
    abs(X - 2.25) < 0.001,
    abs(Y - 1.75) < 0.001,
    abs(Z - 2.75) < 0.001,
    write('  PASSED'), nl, !.

test_3x3_system :-
    write('  FAILED'), nl, fail.

% Test 4: Underdetermined system (infinite solutions)
test_infinite_solutions :-
    write('Test 4: Infinite solutions'), nl,
    Matrix = [[1, 1, 1, 6], [2, 1, 3, 14]],
    solve_system(Matrix, _Solution, SolutionType),
    SolutionType = infinite,
    write('  PASSED'), nl, !.

test_infinite_solutions :-
    write('  FAILED'), nl, fail.

% Test 5: Identity matrix
test_identity :-
    write('Test 5: Identity matrix'), nl,
    Matrix = [[1, 0, 0, 3], [0, 1, 0, 2], [0, 0, 1, 1]],
    solve_system(Matrix, Solution, SolutionType),
    SolutionType = unique,
    Solution = [X, Y, Z],
    abs(X - 3.0) < 0.001,
    abs(Y - 2.0) < 0.001,
    abs(Z - 1.0) < 0.001,
    write('  PASSED'), nl, !.

test_identity :-
    write('  FAILED'), nl, fail.

run_all_tests :-
    write('======================================'), nl,
    write('Gaussian Elimination Solver Tests'), nl,
    write('======================================'), nl, nl,
    
    catch(test_unique_solution_1, E1, (write('Test 1 error: '), write(E1), nl)),
    catch(test_2x2_system, E2, (write('Test 2 error: '), write(E2), nl)),
    catch(test_3x3_system, E3, (write('Test 3 error: '), write(E3), nl)),
    catch(test_infinite_solutions, E4, (write('Test 4 error: '), write(E4), nl)),
    catch(test_identity, E5, (write('Test 5 error: '), write(E5), nl)),
    
    nl,
    write('======================================'), nl,
    write('All tests completed'), nl,
    write('======================================'), nl.

:- initialization(run_all_tests, main).
