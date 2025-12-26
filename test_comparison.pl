:- use_module(starlog_in_prolog).

test1 :-
    write('Test 1: X is [1] & A, X is B & [2]'), nl,
    X is [1] & A,
    write('  After first is: X = '), write(X), write(', A = '), write(A), nl,
    X is B & [2],
    write('  After second is: X = '), write(X), write(', B = '), write(B), nl,
    write('  ✓ Pattern works!'), nl.

test2 :-
    write('Test 2: ([1] & A) is (B & [2])'), nl,
    ([1] & A) is (B & [2]),
    write('  A = '), write(A), write(', B = '), write(B), nl,
    write('  ✓ Pattern works!'), nl.

run_all :-
    catch(test1, E1, (write('✗ Test 1 failed: '), write(E1), nl)), nl,
    catch(test2, E2, (write('✗ Test 2 failed: '), write(E2), nl)), nl.

:- initialization(run_all, main).
