% test_problem_statement_complete.pl
% Comprehensive tests for the problem statement: "Complete ([A•b] & [d]) is [a•B,d]. and all configurations and combinations."
% This tests dual list append expressions with concat operations on both sides

:- use_module('../starlog').

% =====================================================================
% BASIC ATOM CONCATENATION IN LISTS - DUAL EXPRESSIONS
% =====================================================================

test_atom_concat_dual_basic :-
    write('Test 1: ([A•b] & [d]) is [a•B, d] - basic atom concat dual...'),
    (([A•b] & [d]) is [a•B, d]),
    assertion(A = a),
    assertion(B = b),
    % Verify
    atom_concat(A, b, AB1),
    atom_concat(a, B, AB2),
    assertion(AB1 = AB2),
    write(' ✓'), nl.

test_atom_concat_dual_reversed :-
    write('Test 2: ([a•A] & [d]) is [B•b, d] - reversed variables...'),
    (([a•A] & [d]) is [B•b, d]),
    assertion(A = b),
    assertion(B = a),
    write(' ✓'), nl.

test_atom_concat_dual_different_suffix :-
    write('Test 3: ([A•x] & [y]) is [p•B, y] - different atoms...'),
    (([A•x] & [y]) is [p•B, y]),
    assertion(A = p),
    assertion(B = x),
    write(' ✓'), nl.

test_atom_concat_dual_multiple_elements :-
    write('Test 4: ([A•b, c] & [d]) is [a•B, c, d] - multiple elements...'),
    (([A•b, c] & [d]) is [a•B, c, d]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% =====================================================================
% STRING CONCATENATION IN LISTS - DUAL EXPRESSIONS
% =====================================================================

test_string_concat_dual_basic :-
    write('Test 5: ([A:"b"] & ["d"]) is ["a":B, "d"] - basic string concat dual...'),
    (([A:"b"] & ["d"]) is ["a":B, "d"]),
    assertion(A = "a"),
    assertion(B = "b"),
    % Verify
    string_concat(A, "b", AB1),
    string_concat("a", B, AB2),
    assertion(AB1 = AB2),
    write(' ✓'), nl.

test_string_concat_dual_reversed :-
    write('Test 6: (["a":A] & ["d"]) is [B:"b", "d"] - reversed variables...'),
    ((["a":A] & ["d"]) is [B:"b", "d"]),
    assertion(A = "b"),
    assertion(B = "a"),
    write(' ✓'), nl.

test_string_concat_dual_different_values :-
    write('Test 7: ([A:"x"] & ["y"]) is ["p":B, "y"] - different strings...'),
    (([A:"x"] & ["y"]) is ["p":B, "y"]),
    assertion(A = "p"),
    assertion(B = "x"),
    write(' ✓'), nl.

% =====================================================================
% MIXED POSITIONS AND CONFIGURATIONS
% =====================================================================

test_variable_in_different_positions :-
    write('Test 8: ([A•b, c] & [d, e]) is [a•B, c, d, e] - multiple elements both sides...'),
    (([A•b, c] & [d, e]) is [a•B, c, d, e]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

test_both_sides_multiple_concat :-
    write('Test 9: ([A•b] & [c•d]) is [a•B, C•D] - concat on both append parts...'),
    (([A•b] & [c•d]) is [a•B, C•D]),
    assertion(A = a),
    assertion(B = b),
    assertion(C = c),
    assertion(D = d),
    write(' ✓'), nl.

test_concat_in_second_list :-
    write('Test 10: ([a] & [A•b]) is [a, c•B] - concat in right part of append...'),
    (([a] & [A•b]) is [a, c•B]),
    assertion(A = c),
    assertion(B = b),
    write(' ✓'), nl.

% =====================================================================
% EDGE CASES AND COMPLEX PATTERNS
% =====================================================================

test_empty_list_with_concat :-
    write('Test 11: ([A•b] & []) is [a•B] - empty list append...'),
    (([A•b] & []) is [a•B]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

test_single_element_both_sides :-
    write('Test 12: ([A•b]) is [a•B] - single element (implicit empty append)...'),
    % Note: This should work as a simple concat dual expression
    ((A•b) is (a•B)),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

test_triple_concat :-
    write('Test 13: ([A•b•c] & [d]) is [a•B•c, d] - nested concat...'),
    (([A•b•c] & [d]) is [a•B•c, d]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% =====================================================================
% MIXED OPERATORS (ATOM AND STRING)
% =====================================================================

test_mixed_concat_types :-
    write('Test 14: ([A•b] & ["c"]) is [a•B, "c"] - mixed atom and string...'),
    (([A•b] & ["c"]) is [a•B, "c"]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

% =====================================================================
% LONGER LISTS
% =====================================================================

test_longer_lists :-
    write('Test 15: ([A•b, x, y] & [z]) is [a•B, x, y, z] - longer lists...'),
    (([A•b, x, y] & [z]) is [a•B, x, y, z]),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

test_multiple_concat_in_list :-
    write('Test 16: ([A•b, C•d] & [e]) is [a•B, c•D, e] - multiple concat in same list...'),
    (([A•b, C•d] & [e]) is [a•B, c•D, e]),
    assertion(A = a),
    assertion(B = b),
    assertion(C = c),
    assertion(D = d),
    write(' ✓'), nl.

% =====================================================================
% COMPATIBILITY WITH EXISTING FEATURES
% =====================================================================

test_compatibility_simple_dual_concat :-
    write('Test 17: (A•b) is (a•B) - ensure simple dual concat still works...'),
    ((A•b) is (a•B)),
    assertion(A = a),
    assertion(B = b),
    write(' ✓'), nl.

test_compatibility_lhs_only_concat :-
    write('Test 18: ([A•b] & [d]) is [ab, d] - LHS concat only still works...'),
    (([A•b] & [d]) is [ab, d]),
    assertion(A = a),
    write(' ✓'), nl.

test_compatibility_simple_append :-
    write('Test 19: ([a,b] & [c,d]) is [a,b,c,d] - simple append still works...'),
    (([a,b] & [c,d]) is [a,b,c,d]),
    write(' ✓'), nl.

% =====================================================================
% RUN ALL TESTS
% =====================================================================

run_tests :-
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('Problem Statement Complete: ([A•b] & [d]) is [a•B, d]'), nl,
    write('Testing all configurations and combinations'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('BASIC ATOM CONCATENATION IN LISTS'), nl,
    catch(test_atom_concat_dual_basic, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_atom_concat_dual_reversed, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_atom_concat_dual_different_suffix, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_atom_concat_dual_multiple_elements, E4, (write('✗ FAILED: '), write(E4), nl)),
    nl,
    
    write('STRING CONCATENATION IN LISTS'), nl,
    catch(test_string_concat_dual_basic, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_string_concat_dual_reversed, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_string_concat_dual_different_values, E7, (write('✗ FAILED: '), write(E7), nl)),
    nl,
    
    write('MIXED POSITIONS AND CONFIGURATIONS'), nl,
    catch(test_variable_in_different_positions, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_both_sides_multiple_concat, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_concat_in_second_list, E10, (write('✗ FAILED: '), write(E10), nl)),
    nl,
    
    write('EDGE CASES AND COMPLEX PATTERNS'), nl,
    catch(test_empty_list_with_concat, E11, (write('✗ FAILED: '), write(E11), nl)),
    catch(test_single_element_both_sides, E12, (write('✗ FAILED: '), write(E12), nl)),
    catch(test_triple_concat, E13, (write('✗ FAILED: '), write(E13), nl)),
    nl,
    
    write('MIXED OPERATORS'), nl,
    catch(test_mixed_concat_types, E14, (write('✗ FAILED: '), write(E14), nl)),
    nl,
    
    write('LONGER LISTS'), nl,
    catch(test_longer_lists, E15, (write('✗ FAILED: '), write(E15), nl)),
    catch(test_multiple_concat_in_list, E16, (write('✗ FAILED: '), write(E16), nl)),
    nl,
    
    write('COMPATIBILITY WITH EXISTING FEATURES'), nl,
    catch(test_compatibility_simple_dual_concat, E17, (write('✗ FAILED: '), write(E17), nl)),
    catch(test_compatibility_lhs_only_concat, E18, (write('✗ FAILED: '), write(E18), nl)),
    catch(test_compatibility_simple_append, E19, (write('✗ FAILED: '), write(E19), nl)),
    nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('All Problem Statement Tests Complete! ✓'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(run_tests, main).
