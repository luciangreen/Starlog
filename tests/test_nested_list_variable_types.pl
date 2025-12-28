% test_nested_list_variable_types.pl
% Tests for variable types in nested vs. non-nested lists with concat operators
% This tests the requirement that:
%   - In [A:a] is [a:a], A should be atom 'a'
%   - In [[A:a]] is [[a:a]], A should be atom 'a' (consistent with single-level behavior)

:- use_module('../starlog').

% Test 1: Single-level list with colon operator produces atom
test_single_level_atom :-
    write('Test 1: [A:a] is [a:a] should produce A = a (atom)...'),
    starlog_call([A:a] is [a:a]),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 2: Nested list with colon operator produces atom (consistent with single-level)
test_nested_level_string :-
    write('Test 2: [[A:a]] is [[a:a]] should produce A = a (atom)...'),
    starlog_call([[A:a]] is [[a:a]]),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 3: Single-level list with atom concat operator produces atom
test_single_level_atom_concat :-
    write('Test 3: [A•a] is [a•a] should produce A = a (atom)...'),
    starlog_call([A•a] is [a•a]),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 4: Nested list with atom concat operator produces atom
test_nested_level_atom_concat :-
    write('Test 4: [[A•a]] is [[a•a]] should produce A = a (atom)...'),
    starlog_call([[A•a]] is [[a•a]]),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 5: Triple-nested list with colon operator produces atom (consistent with other levels)
test_triple_nested_string :-
    write('Test 5: [[[A:a]]] is [[[a:a]]] should produce A = a (atom)...'),
    starlog_call([[[A:a]]] is [[[a:a]]]),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 6: Multiple elements in single-level list
test_multiple_elements_single_level :-
    write('Test 6: [A:a, B:b] is [a:a, b:b] should produce atoms...'),
    starlog_call([A:a, B:b] is [a:a, b:b]),
    assertion(atom(A)),
    assertion(atom(B)),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% Test 7: Multiple elements in nested list - produces atoms (consistent with single-level)
test_multiple_elements_nested :-
    write('Test 7: [[A:a], [B:b]] is [[a:a], [b:b]] should produce atoms...'),
    starlog_call([[A:a], [B:b]] is [[a:a], [b:b]]),
    assertion(atom(A)),
    assertion(atom(B)),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% Test 8: Mixed nesting levels
% Note: In mixed nesting, the nested list elements are unified directly,
% so B ends up being an atom (from b:b being compiled as atom concat)
test_mixed_nesting :-
    write('Test 8: [A:a, [B:b]] should handle mixed nesting correctly...'),
    starlog_call([A:a, [B:b]] is [a:a, [b:b]]),
    assertion(atom(A)),
    assertion(atom(B)),  % B is atom because of direct unification
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% Test 9: Verify that a:a alone (without list) produces atom
test_bare_concat_atom :-
    write('Test 9: (A:a) is (a:a) without list should produce atom...'),
    starlog_call((A:a) is (a:a)),
    assertion(atom(A)),
    assertion(A = a),
    write(' ✓'), nl.

% Test 10: Verify correct result values in single level
test_result_value_single :-
    write('Test 10: [x:y] should produce "xy" (string) in list...'),
    starlog_call(R is [x:y]),
    assertion(R = ["xy"]),
    write(' ✓'), nl.

% Test 11: Verify correct result values in nested level  
test_result_value_nested :-
    write('Test 11: [[x:y]] should produce nested string...'),
    starlog_call(R is [[x:y]]),
    assertion(R = [["xy"]]),
    write(' ✓'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing Nested List Variable Types'), nl,
    write('Problem: Ensure correct type handling in nested vs non-nested lists'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_single_level_atom, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_nested_level_string, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_single_level_atom_concat, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_nested_level_atom_concat, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_triple_nested_string, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_multiple_elements_single_level, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_multiple_elements_nested, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_mixed_nesting, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_bare_concat_atom, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_result_value_single, E10, (write('✗ FAILED: '), write(E10), nl)),
    catch(test_result_value_nested, E11, (write('✗ FAILED: '), write(E11), nl)),
    nl,
    
    write('==================================================================='), nl,
    write('All Nested List Variable Type Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
