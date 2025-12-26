% test_integrated_algebra.pl
% Test algebra solver through the main starlog_in_prolog module

:- use_module('../starlog_in_prolog').

test_integrated_solve :-
    write('Test: Integrated algebra solver through starlog_in_prolog...'),
    solve_equation((Y+5)/2 is 2, Y, Solution),
    assertion(Solution =:= -1),
    write(' ✓ (Y = -1)'), nl.

test_integrated_multiple :-
    write('Test: Multiple equations...'),
    solve_equation(2*X is 10, X, S1),
    solve_equation(Y-3 is 7, Y, S2),
    solve_equation((Z+1)/2 is 5, Z, S3),
    assertion(S1 =:= 5),
    assertion(S2 =:= 10),
    assertion(S3 =:= 9),
    write(' ✓'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing Integrated Algebra Solver'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_integrated_solve, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_integrated_multiple, E2, (write('✗ FAILED: '), write(E2), nl)),
    
    nl,
    write('==================================================================='), nl,
    write('Integrated Algebra Solver Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
