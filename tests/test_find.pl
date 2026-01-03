% test_find.pl
% Tests for the find/2 predicate (Starlog syntax)
% Starlog syntax: Result is find(Template, Goal)

:- use_module('../starlog').

% Test 1: Basic example from problem statement
test_basic_example :-
    write('Test 1: Result is find(A, starlog_call([A:a] is [a:a]))...'),
    starlog_call(Result is find(A, starlog_call([A:a] is [a:a]))),
    assertion(Result = a),
    assertion(atom(Result)),
    write(' ✓'), nl.

% Test 2: Simple member test
test_member :-
    write('Test 2: Result is find(X, member(X, [1,2,3]))...'),
    starlog_call(Result is find(X, member(X, [1,2,3]))),
    assertion(Result = 1),
    write(' ✓'), nl.

% Test 3: String concatenation with Starlog
test_string_concat :-
    write('Test 3: Result is find(R, starlog_call(R is "hello":"world"))...'),
    starlog_call(Result is find(R, starlog_call(R is "hello":"world"))),
    assertion(Result = "helloworld"),
    write(' ✓'), nl.

% Test 4: List append with Starlog
test_list_append :-
    write('Test 4: Result is find(L, starlog_call(L is [1,2]&[3,4]))...'),
    starlog_call(Result is find(L, starlog_call(L is [1,2]&[3,4]))),
    assertion(Result = [1,2,3,4]),
    write(' ✓'), nl.

% Test 5: Arithmetic
test_arithmetic :-
    write('Test 5: Result is find(X, X is 2+3)...'),
    starlog_call(Result is find(X, X is 2+3)),
    assertion(Result = 5),
    write(' ✓'), nl.

% Test 6: Multiple solutions - should only get first due to cut
test_cut_behavior :-
    write('Test 6: find with multiple solutions (cut behavior)...'),
    starlog_call(Result is find(X, member(X, [a,b,c,d]))),
    assertion(Result = a),
    write(' ✓'), nl.

% Test 7: Nested list pattern from problem statement
test_nested_list :-
    write('Test 7: Result is find(A, starlog_call([A•a] is [b•a]))...'),
    starlog_call(Result is find(A, starlog_call([A•a] is [b•a]))),
    assertion(Result = b),
    assertion(atom(Result)),
    write(' ✓'), nl.

% Test 8: Dual expression pattern
test_dual_expression :-
    write('Test 8: Result is find(A, starlog_call(([1]&A) is (B&[2])))...'),
    starlog_call(Result is find(A, starlog_call(([1]&A) is (B&[2])))),
    assertion(Result = [2]),
    write(' ✓'), nl.

% Test 9: Compound goal
test_compound_goal :-
    write('Test 9: Result is find(X, (member(Y, [1,2,3]), X is Y * 2))...'),
    starlog_call(Result is find(X, (member(Y, [1,2,3]), X is Y * 2))),
    assertion(Result = 2),
    write(' ✓'), nl.

% Test 10: Reverse operation
test_reverse :-
    write('Test 10: Result is find(R, starlog_call(R is reverse([1,2,3])))...'),
    starlog_call(Result is find(R, starlog_call(R is reverse([1,2,3])))),
    assertion(Result = [3,2,1]),
    write(' ✓'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing find/2 Predicate (Starlog syntax)'), nl,
    write('Starlog syntax: Result is find(Template, Goal)'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_basic_example, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_member, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_string_concat, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_list_append, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_arithmetic, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_cut_behavior, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_nested_list, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_dual_expression, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_compound_goal, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_reverse, E10, (write('✗ FAILED: '), write(E10), nl)),
    nl,
    
    write('==================================================================='), nl,
    write('All find/2 Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
