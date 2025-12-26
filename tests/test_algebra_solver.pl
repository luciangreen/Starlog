% test_algebra_solver.pl
% Tests for the algebraic equation solver

:- use_module('../algebra_solver').

% Test 1: Basic equation from problem statement
% (Y+5)/2 is 2, should solve to Y = -1
test_basic_equation :-
    write('Test 1: (Y+5)/2 = 2...'),
    solve_equation((Y+5)/2 is 2, Y, Solution),
    assertion(Solution =:= -1),
    write(' ✓ (Y = -1)'), nl.

% Test 2: Simple addition
% Y+3 is 7, should solve to Y = 4
test_simple_addition :-
    write('Test 2: Y+3 = 7...'),
    solve_equation(Y+3 is 7, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 3: Simple subtraction
% Y-2 is 5, should solve to Y = 7
test_simple_subtraction :-
    write('Test 3: Y-2 = 5...'),
    solve_equation(Y-2 is 5, Y, Solution),
    assertion(Solution =:= 7),
    write(' ✓ (Y = 7)'), nl.

% Test 4: Simple multiplication
% Y*3 is 12, should solve to Y = 4
test_simple_multiplication :-
    write('Test 4: Y*3 = 12...'),
    solve_equation(Y*3 is 12, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 5: Simple division
% Y/4 is 2, should solve to Y = 8
test_simple_division :-
    write('Test 5: Y/4 = 2...'),
    solve_equation(Y/4 is 2, Y, Solution),
    assertion(Solution =:= 8),
    write(' ✓ (Y = 8)'), nl.

% Test 6: Commutative operations - addition
% 5+Y is 9, should solve to Y = 4
test_commutative_addition :-
    write('Test 6: 5+Y = 9...'),
    solve_equation(5+Y is 9, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 7: Commutative operations - multiplication
% 2*Y is 10, should solve to Y = 5
test_commutative_multiplication :-
    write('Test 7: 2*Y = 10...'),
    solve_equation(2*Y is 10, Y, Solution),
    assertion(Solution =:= 5),
    write(' ✓ (Y = 5)'), nl.

% Test 8: Multi-step equation
% 2*Y+3 is 11, should solve to Y = 4
test_multi_step :-
    write('Test 8: 2*Y+3 = 11...'),
    solve_equation(2*Y+3 is 11, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 9: Complex nested equation
% (Y+1)*2 is 10, should solve to Y = 4
test_nested_equation :-
    write('Test 9: (Y+1)*2 = 10...'),
    solve_equation((Y+1)*2 is 10, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 10: Division and addition combined
% (Y+2)/3 is 4, should solve to Y = 10
test_division_addition :-
    write('Test 10: (Y+2)/3 = 4...'),
    solve_equation((Y+2)/3 is 4, Y, Solution),
    assertion(Solution =:= 10),
    write(' ✓ (Y = 10)'), nl.

% Test 11: Subtraction from constant
% 10-Y is 3, should solve to Y = 7
test_subtraction_from_constant :-
    write('Test 11: 10-Y = 3...'),
    solve_equation(10-Y is 3, Y, Solution),
    assertion(Solution =:= 7),
    write(' ✓ (Y = 7)'), nl.

% Test 12: Power equation
% Y**2 is 16, should solve to Y = 4
test_power :-
    write('Test 12: Y**2 = 16...'),
    solve_equation(Y**2 is 16, Y, Solution),
    assertion(Solution =:= 4),
    write(' ✓ (Y = 4)'), nl.

% Test 13: Variable on right side
% 2 is Y/3, should solve to Y = 6
test_variable_on_right :-
    write('Test 13: 2 = Y/3...'),
    solve_equation(2 is Y/3, Y, Solution),
    assertion(Solution =:= 6),
    write(' ✓ (Y = 6)'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing Algebra Solver'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_basic_equation, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_simple_addition, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_simple_subtraction, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_simple_multiplication, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_simple_division, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_commutative_addition, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_commutative_multiplication, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_multi_step, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_nested_equation, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_division_addition, E10, (write('✗ FAILED: '), write(E10), nl)),
    catch(test_subtraction_from_constant, E11, (write('✗ FAILED: '), write(E11), nl)),
    catch(test_power, E12, (write('✗ FAILED: '), write(E12), nl)),
    catch(test_variable_on_right, E13, (write('✗ FAILED: '), write(E13), nl)),
    
    nl,
    write('==================================================================='), nl,
    write('All Algebra Solver Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
