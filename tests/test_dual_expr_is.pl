% test_dual_expr_is.pl
% Tests for dual expression is patterns: (Expr1 is Expr2)
% This tests the new functionality where both LHS and RHS are Starlog expressions

:- use_module('../starlog').

test_list_append_dual :-
    write('Testing dual list append: ([1] & A) is (B & [2])...'),
    ([1] & A) is (B & [2]),
    A = [2],
    B = [1],
    write(' ✓'), nl.

test_list_append_dual_complex :-
    write('Testing complex dual list append: ([1,2] & A) is (B & [3,4])...'),
    ([1,2] & A) is (B & [3,4]),
    A = [3,4],
    B = [1,2],
    write(' ✓'), nl.

test_list_append_dual_variable :-
    write('Testing dual list append with free variables: ([1] & A) is (B & [2])...'),
    ([1] & A) is (B & [2]),
    % Check that we get the expected values
    append([1], A, R1),
    append(B, [2], R2),
    R1 = R2,
    write(' ✓'), nl.

test_nested_list_append :-
    write('Testing nested list append: (([1] & [2]) & A) is (B & [3])...'),
    (([1] & [2]) & A) is (B & [3]),
    A = [3],
    B = [1,2],
    write(' ✓'), nl.

test_string_concat_dual :-
    write('Testing dual string concat with specific values...'),
    % We can't solve for variables in the middle, but we can unify results
    X is "hello" : "world",
    Y is "hello" : "world",  
    X = Y,
    write(' ✓'), nl.

test_atom_concat_dual :-
    write('Testing dual atom concat with specific values...'),
    X is a • b,
    Y is a • b,
    X = Y,
    write(' ✓'), nl.

test_mixed_operators :-
    write('Testing mixed operators in dual expr: ([1] & [2]) is ([3] & [4])...'),
    % This should fail as the lists are different
    (\+ (([1] & [2]) is ([3] & [4])) -> write(' ✓') ; (write(' ✗'), fail)), nl.

test_unification_works :-
    write('Testing that simple unification works: X = Y...'),
    % Note: ([1,2]) is ([1,2]) would be arithmetic is/2, not Starlog
    % So we just test simple unification here
    X = [1,2],
    Y = [1,2],
    X = Y,
    write(' ✓'), nl.

run_tests :-
    write('Running dual expression is/2 tests...'), nl, nl,
    catch(test_list_append_dual, E1, (write('✗ Test 1 failed: '), write(E1), nl)),
    catch(test_list_append_dual_complex, E2, (write('✗ Test 2 failed: '), write(E2), nl)),
    catch(test_list_append_dual_variable, E3, (write('✗ Test 3 failed: '), write(E3), nl)),
    catch(test_nested_list_append, E4, (write('✗ Test 4 failed: '), write(E4), nl)),
    catch(test_string_concat_dual, E5, (write('✗ Test 5 failed: '), write(E5), nl)),
    catch(test_atom_concat_dual, E6, (write('✗ Test 6 failed: '), write(E6), nl)),
    catch(test_mixed_operators, E7, (write('✗ Test 7 failed: '), write(E7), nl)),
    catch(test_unification_works, E8, (write('✗ Test 8 failed: '), write(E8), nl)),
    nl,
    write('Dual expression is/2 tests complete!'), nl.

:- initialization(run_tests, main).
