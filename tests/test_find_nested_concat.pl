% test_find_nested_concat.pl
% Tests for find/3 with nested concatenation expressions
% This tests the enhancement to support patterns like [A:a:C] is [a:a:c]

:- use_module('../starlog').

% Test 1: Basic three-way string concatenation from problem statement
test_problem_statement :-
    write('Test 1: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)...'),
    find([A,C], starlog_call([A:a:C] is [a:a:c]), Result),
    assertion(Result = [a,c]),
    write(' ✓'), nl.

% Test 2: Three-way concatenation with different middle value
test_three_way_concat :-
    write('Test 2: find([A,C], starlog_call([A:x:C] is [y:x:z]), Result)...'),
    find([A,C], starlog_call([A:x:C] is [y:x:z]), Result),
    assertion(Result = [y,z]),
    write(' ✓'), nl.

% Test 3: Four-way concatenation
test_four_way_concat :-
    write('Test 3: find([A,C], starlog_call([A:b:C:d] is [a:b:c:d]), Result)...'),
    find([A,C], starlog_call([A:b:C:d] is [a:b:c:d]), Result),
    assertion(Result = [a,c]),
    write(' ✓'), nl.

% Test 4: Variables at end
test_variables_at_end :-
    write('Test 4: find([B,C], starlog_call([a:B:C] is [a:b:c]), Result)...'),
    find([B,C], starlog_call([a:B:C] is [a:b:c]), Result),
    assertion(Result = [b,c]),
    write(' ✓'), nl.

% Test 5: Direct three-way concatenation (not in list)
test_direct_concat :-
    write('Test 5: starlog_call((A:a:C) is (b:a:c))...'),
    starlog_call((A:a:C) is (b:a:c)),
    assertion(A = b),
    assertion(C = c),
    write(' ✓'), nl.

% Test 6: Atom concatenation with three parts
test_atom_concat_three_way :-
    write('Test 6: find([A,C], starlog_call([A•x•C] is [y•x•z]), Result)...'),
    find([A,C], starlog_call([A•x•C] is [y•x•z]), Result),
    assertion(Result = [y,z]),
    write(' ✓'), nl.

% Test 7: Reverse - variables on right side
test_reverse_variables :-
    write('Test 7: starlog_call([a:b:c] is [A:b:C])...'),
    starlog_call([a:b:c] is [A:b:C]),
    assertion(A = a),
    assertion(C = c),
    write(' ✓'), nl.

% Test 8: Five-way concatenation
test_five_way_concat :-
    write('Test 8: find([A,C,E], starlog_call([A:b:C:d:E] is [a:b:c:d:e]), Result)...'),
    find([A,C,E], starlog_call([A:b:C:d:E] is [a:b:c:d:e]), Result),
    assertion(Result = [a,c,e]),
    write(' ✓'), nl.

% Test 9: Mixed with different fixed values
test_mixed_fixed_values :-
    write('Test 9: find([A,C], starlog_call([A:"_":C] is ["hello":"_":"world"]), Result)...'),
    find([A,C], starlog_call([A:"_":C] is ["hello":"_":"world"]), Result),
    assertion(Result = ["hello","world"]),
    write(' ✓'), nl.

% Test 10: Nested in atom concatenation
test_nested_atom_concat :-
    write('Test 10: find([A,C], starlog_call([A•m•C] is [x•m•y]), Result)...'),
    find([A,C], starlog_call([A•m•C] is [x•m•y]), Result),
    assertion(Result = [x,y]),
    write(' ✓'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing find/3 with Nested Concatenation'), nl,
    write('Problem: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_problem_statement, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_three_way_concat, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_four_way_concat, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_variables_at_end, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_direct_concat, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_atom_concat_three_way, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_reverse_variables, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_five_way_concat, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_mixed_fixed_values, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_nested_atom_concat, E10, (write('✗ FAILED: '), write(E10), nl)),
    nl,
    
    write('==================================================================='), nl,
    write('All Nested Concatenation Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
