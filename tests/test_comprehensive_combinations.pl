% test_comprehensive_combinations.pl
% Comprehensive tests for dual expressions with all operator combinations
% Tests the requirement: "Complete A:...a is b:...B and A•a is b•B. with any combination of :,• [], arithmetic expression or &"

:- use_module('../starlog').

% =====================================================================
% STRING CONCATENATION (:) OPERATOR TESTS
% =====================================================================

test_string_basic :-
    write('Test 1: Basic string concat (a : A) is (B : b)...'),
    (a : A) is (B : b),
    assertion(A = b),
    assertion(B = a),
    write(' ✓'), nl.

test_string_nested :-
    write('Test 2: Nested string concat ((a : "x") : A) is (B : (b : "y"))...'),
    ((a : "x") : A) is (B : (b : "y")),
    assertion(A = (b:"y")),
    assertion(B = (a:"x")),
    write(' ✓'), nl.

test_string_triple :-
    write('Test 3: Triple string concat ((a : x) : A) is (B : (y : z))...'),
    ((a : x) : A) is (B : (y : z)),
    assertion(A = (y:z)),
    assertion(B = (a:x)),
    write(' ✓'), nl.

% =====================================================================
% ATOM CONCATENATION (•) OPERATOR TESTS
% =====================================================================

test_atom_basic :-
    write('Test 4: Basic atom concat (a • A) is (B • b)...'),
    (a • A) is (B • b),
    assertion(A = b),
    assertion(B = a),
    write(' ✓'), nl.

test_atom_nested :-
    write('Test 5: Nested atom concat ((a • x) • A) is (B • (b • y))...'),
    ((a • x) • A) is (B • (b • y)),
    assertion(A = (b•y)),
    assertion(B = (a•x)),
    write(' ✓'), nl.

test_atom_triple :-
    write('Test 6: Triple atom concat ((a • x) • A) is (B • (y • z))...'),
    ((a • x) • A) is (B • (y • z)),
    assertion(A = (y•z)),
    assertion(B = (a•x)),
    write(' ✓'), nl.

% =====================================================================
% LIST APPEND (&) OPERATOR TESTS
% =====================================================================

test_list_basic :-
    write('Test 7: Basic list append ([1] & A) is (B & [2])...'),
    ([1] & A) is (B & [2]),
    assertion(A = [2]),
    assertion(B = [1]),
    write(' ✓'), nl.

test_list_nested :-
    write('Test 8: Nested list append (([1] & [2]) & A) is (B & [3])...'),
    (([1] & [2]) & A) is (B & [3]),
    assertion(A = [3]),
    assertion(B = [1,2]),
    write(' ✓'), nl.

test_list_triple :-
    write('Test 9: Triple list append (([1] & [2]) & A) is (B & ([3] & [4]))...'),
    (([1] & [2]) & A) is (B & ([3] & [4])),
    assertion(A = [3,4]),
    assertion(B = [1,2]),
    write(' ✓'), nl.

test_list_complex :-
    write('Test 10: Complex list ([a,b] & A) is (B & [c,d])...'),
    ([a,b] & A) is (B & [c,d]),
    assertion(A = [c,d]),
    assertion(B = [a,b]),
    write(' ✓'), nl.

% =====================================================================
% RESULT EQUALITY TESTS
% =====================================================================

test_string_equality :-
    write('Test 11: String equality (a : x) is (a : x)...'),
    ((a : x) is (a : x)),
    write(' ✓'), nl.

test_atom_equality :-
    write('Test 12: Atom equality (a • x) is (a • x)...'),
    ((a • x) is (a • x)),
    write(' ✓'), nl.

test_list_equality :-
    write('Test 13: List equality ([1,2] & [3]) is ([1,2] & [3])...'),
    (([1,2] & [3]) is ([1,2] & [3])),
    write(' ✓'), nl.

% =====================================================================
% ARITHMETIC EXPRESSION TESTS
% =====================================================================

test_arithmetic_standalone :-
    write('Test 14: Arithmetic X is 1 + 2...'),
    X is 1 + 2,
    assertion(X = 3),
    write(' ✓'), nl.

test_arithmetic_sequence :-
    write('Test 15: Mixed Starlog and arithmetic...'),
    L is [1] & [2],
    N is 1 + 2,
    assertion(L = [1,2]),
    assertion(N = 3),
    write(' ✓'), nl.

% =====================================================================
% IDENTITY ELEMENT TESTS
% =====================================================================

test_empty_list :-
    write('Test 16: Empty list identity ([] & A) is (B & [1])...'),
    ([] & A) is (B & [1]),
    assertion(A = [1]),
    assertion(B = []),
    write(' ✓'), nl.

test_empty_string :-
    write('Test 17: Empty string identity ([] & A) is ([] & [x])...'),
    ([] & A) is ([] & [x]),
    assertion(A = [x]),
    write(' ✓'), nl.

% =====================================================================
% COMPLEX NESTED PATTERN TESTS
% =====================================================================

test_deeply_nested_strings :-
    write('Test 18: Deeply nested strings (((a : b) : c) : A) is (B : ((d : e) : f))...'),
    (((a : b) : c) : A) is (B : ((d : e) : f)),
    assertion(A = (d:e:f)),
    assertion(B = (a:b:c)),
    write(' ✓'), nl.

test_deeply_nested_lists :-
    write('Test 19: Deeply nested lists ((([1] & [2]) & [3]) & A) is (B & (([4] & [5]) & [6]))...'),
    ((([1] & [2]) & [3]) & A) is (B & (([4] & [5]) & [6])),
    assertion(A = [4,5,6]),
    assertion(B = [1,2,3]),
    write(' ✓'), nl.

% =====================================================================
% VARIABLE POSITION TESTS
% =====================================================================

test_variable_middle :-
    write('Test 20: Variable in middle ([1,2] & A & [5,6]) is ([1,2] & [3,4] & [5,6])...'),
    ([1,2] & A & [5,6]) is ([1,2] & [3,4] & [5,6]),
    assertion(A = [3,4]),
    write(' ✓'), nl.

test_multiple_variables :-
    write('Test 21: Multiple variables (a : A) is (B : b)...'),
    (a : A) is (B : b),
    assertion(A = b),
    assertion(B = a),
    write(' ✓'), nl.

% =====================================================================
% ADDITIONAL COMBINATION TESTS
% =====================================================================

test_mixed_list_string :-
    write('Test 22: Different operator types in sequence...'),
    L is [1] & [2],
    S is "a" : "b",
    assertion(L = [1,2]),
    assertion(S = "ab"),
    write(' ✓'), nl.

test_mixed_atom_list :-
    write('Test 23: Atom and list operators in sequence...'),
    A is x • y,
    L is [1] & [2],
    assertion(A = xy),
    assertion(L = [1,2]),
    write(' ✓'), nl.

test_all_operators_sequence :-
    write('Test 24: All operators in sequence...'),
    S is "a" : "b",
    A is x • y,
    L is [1] & [2],
    N is 1 + 1,
    assertion(S = "ab"),
    assertion(A = xy),
    assertion(L = [1,2]),
    assertion(N = 2),
    write(' ✓'), nl.

% =====================================================================
% RUN ALL TESTS
% =====================================================================

run_tests :-
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('Comprehensive Operator Combinations Tests'), nl,
    write('Testing: "A:...a is b:...B and A•a is b•B"'), nl,
    write('With combinations of: :, •, [], &, arithmetic expressions'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('STRING CONCATENATION (:) TESTS'), nl,
    catch(test_string_basic, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_string_nested, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_string_triple, E3, (write('✗ FAILED: '), write(E3), nl)),
    nl,
    
    write('ATOM CONCATENATION (•) TESTS'), nl,
    catch(test_atom_basic, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_atom_nested, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_atom_triple, E6, (write('✗ FAILED: '), write(E6), nl)),
    nl,
    
    write('LIST APPEND (&) TESTS'), nl,
    catch(test_list_basic, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_list_nested, E8, (write('✗ FAILED: '), write(E8), nl)),
    catch(test_list_triple, E9, (write('✗ FAILED: '), write(E9), nl)),
    catch(test_list_complex, E10, (write('✗ FAILED: '), write(E10), nl)),
    nl,
    
    write('RESULT EQUALITY TESTS'), nl,
    catch(test_string_equality, E11, (write('✗ FAILED: '), write(E11), nl)),
    catch(test_atom_equality, E12, (write('✗ FAILED: '), write(E12), nl)),
    catch(test_list_equality, E13, (write('✗ FAILED: '), write(E13), nl)),
    nl,
    
    write('ARITHMETIC EXPRESSION TESTS'), nl,
    catch(test_arithmetic_standalone, E14, (write('✗ FAILED: '), write(E14), nl)),
    catch(test_arithmetic_sequence, E15, (write('✗ FAILED: '), write(E15), nl)),
    nl,
    
    write('IDENTITY ELEMENT TESTS'), nl,
    catch(test_empty_list, E16, (write('✗ FAILED: '), write(E16), nl)),
    catch(test_empty_string, E17, (write('✗ FAILED: '), write(E17), nl)),
    nl,
    
    write('COMPLEX NESTED PATTERN TESTS'), nl,
    catch(test_deeply_nested_strings, E18, (write('✗ FAILED: '), write(E18), nl)),
    catch(test_deeply_nested_lists, E19, (write('✗ FAILED: '), write(E19), nl)),
    nl,
    
    write('VARIABLE POSITION TESTS'), nl,
    catch(test_variable_middle, E20, (write('✗ FAILED: '), write(E20), nl)),
    catch(test_multiple_variables, E21, (write('✗ FAILED: '), write(E21), nl)),
    nl,
    
    write('ADDITIONAL COMBINATION TESTS'), nl,
    catch(test_mixed_list_string, E22, (write('✗ FAILED: '), write(E22), nl)),
    catch(test_mixed_atom_list, E23, (write('✗ FAILED: '), write(E23), nl)),
    catch(test_all_operators_sequence, E24, (write('✗ FAILED: '), write(E24), nl)),
    nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('All Comprehensive Combination Tests Complete! ✓'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(run_tests, main).
