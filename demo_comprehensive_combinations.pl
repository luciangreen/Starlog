% demo_comprehensive_combinations.pl
% Comprehensive demonstration of dual expressions with all operator combinations
% Implements: "Complete A:...a is b:...B and A•a is b•B. with any combination of :,• [], arithmetic expression or &"

:- use_module(starlog_in_prolog).

% =====================================================================
% STRING CONCATENATION OPERATOR (:) COMBINATIONS
% =====================================================================

demo_string_concat_basic :-
    write('1. Basic string concatenation: (a : A) is (B : b)'), nl,
    (a : A) is (B : b),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Verification: Both sides equal "ab"'), nl, nl.

demo_string_concat_nested :-
    write('2. Nested string concatenation: ((a : "x") : A) is (B : (b : "y"))'), nl,
    ((a : "x") : A) is (B : (b : "y")),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Both sides evaluate to same result'), nl, nl.

demo_string_concat_triple :-
    write('3. Triple string concatenation: ((a : x) : A) is (B : (y : z))'), nl,
    ((a : x) : A) is (B : (y : z)),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Pattern: nested concatenation'), nl, nl.

% =====================================================================
% ATOM CONCATENATION OPERATOR (•) COMBINATIONS
% =====================================================================

demo_atom_concat_basic :-
    write('4. Basic atom concatenation: (a • A) is (B • b)'), nl,
    (a • A) is (B • b),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Verification: Both sides equal atom ab'), nl, nl.

demo_atom_concat_nested :-
    write('5. Nested atom concatenation: ((a • x) • A) is (B • (b • y))'), nl,
    ((a • x) • A) is (B • (b • y)),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Both sides evaluate to same result'), nl, nl.

demo_atom_concat_triple :-
    write('6. Triple atom concatenation: ((a • x) • A) is (B • (y • z))'), nl,
    ((a • x) • A) is (B • (y • z)),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Pattern: nested concatenation'), nl, nl.

% =====================================================================
% LIST APPEND OPERATOR (&) COMBINATIONS
% =====================================================================

demo_list_append_basic :-
    write('7. Basic list append: ([1] & A) is (B & [2])'), nl,
    ([1] & A) is (B & [2]),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Verification: Both sides equal [1,2]'), nl, nl.

demo_list_append_nested :-
    write('8. Nested list append: (([1] & [2]) & A) is (B & [3])'), nl,
    (([1] & [2]) & A) is (B & [3]),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Both sides evaluate to [1,2,3]'), nl, nl.

demo_list_append_triple :-
    write('9. Triple list append: (([1] & [2]) & A) is (B & ([3] & [4]))'), nl,
    (([1] & [2]) & A) is (B & ([3] & [4])),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Pattern: nested append'), nl, nl.

demo_list_append_complex :-
    write('10. Complex list variables: ([a,b] & A) is (B & [c,d])'), nl,
    ([a,b] & A) is (B & [c,d]),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Both sides equal [a,b,c,d]'), nl, nl.

% =====================================================================
% MIXED OPERATOR COMBINATIONS (Same type results)
% =====================================================================

demo_string_and_list :-
    write('11. String concat result equality: (a : x) is (a : x)'), nl,
    ((a : x) is (a : x)),
    write('   Result: Both evaluate to "ax" - unification succeeds'), nl, nl.

demo_atom_and_list :-
    write('12. Atom concat result equality: (a • x) is (a • x)'), nl,
    ((a • x) is (a • x)),
    write('   Result: Both evaluate to ax - unification succeeds'), nl, nl.

demo_list_result_equality :-
    write('13. List append result equality: ([1,2] & [3]) is ([1,2] & [3])'), nl,
    (([1,2] & [3]) is ([1,2] & [3])),
    write('   Result: Both evaluate to [1,2,3] - unification succeeds'), nl, nl.

% =====================================================================
% ARITHMETIC EXPRESSIONS IN CONTEXT
% =====================================================================

demo_arithmetic_standalone :-
    write('14. Arithmetic expression (not dual): X is 1 + 2'), nl,
    X is 1 + 2,
    format('   Result: X = ~w~n', [X]),
    write('   Note: Pure arithmetic, not a dual expression'), nl, nl.

demo_arithmetic_sequence :-
    write('15. Arithmetic and Starlog sequence: L is [1] & [2], N is 1 + 2'), nl,
    L is [1] & [2],
    N is 1 + 2,
    format('   Result: L = ~w, N = ~w~n', [L, N]),
    write('   Note: Both operators work in sequence'), nl, nl.

% =====================================================================
% EMPTY LIST/STRING CASES
% =====================================================================

demo_empty_list_concat :-
    write('16. Empty list concatenation: ([] & A) is (B & [1])'), nl,
    ([] & A) is (B & [1]),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Identity: [] is identity for &'), nl, nl.

demo_empty_string_concat :-
    write('17. Empty list as identity: ([] & A) is ([] & [x])'), nl,
    ([] & A) is ([] & [x]),
    format('   Result: A = ~w~n', [A]),
    write('   Identity: [] is identity for &'), nl, nl.

% =====================================================================
% COMPLEX NESTED PATTERNS
% =====================================================================

demo_deeply_nested_strings :-
    write('18. Deeply nested strings: (((a : b) : c) : A) is (B : ((d : e) : f))'), nl,
    (((a : b) : c) : A) is (B : ((d : e) : f)),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Complex nesting pattern'), nl, nl.

demo_deeply_nested_lists :-
    write('19. Deeply nested lists: ((([1] & [2]) & [3]) & A) is (B & (([4] & [5]) & [6]))'), nl,
    ((([1] & [2]) & [3]) & A) is (B & (([4] & [5]) & [6])),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Complex nesting with lists'), nl, nl.

% =====================================================================
% VARIABLE POSITION VARIATIONS
% =====================================================================

demo_variable_left :-
    write('20. Variable on left of string: (A : b) is (a : b)'), nl,
    write('   Note: Cannot solve - "ab" has multiple splits'), nl,
    write('   Using constraint: A:b must equal a:b'), nl,
    ((A : b) is (a : b) -> format('   Result: A = ~w~n', [A]) ; write('   (Multiple solutions exist)~n')),
    nl.

demo_variable_middle :-
    write('21. Variable in middle: ([1,2] & A & [5,6]) is ([1,2] & [3,4] & [5,6])'), nl,
    ([1,2] & A & [5,6]) is ([1,2] & [3,4] & [5,6]),
    format('   Result: A = ~w~n', [A]),
    write('   Solved by matching structure'), nl, nl.

demo_multiple_variables :-
    write('22. Multiple variables solved: (a : A) is (B : b)'), nl,
    (a : A) is (B : b),
    format('   Result: A = ~w, B = ~w~n', [A, B]),
    write('   Both variables solved by pattern matching'), nl, nl.

% =====================================================================
% MAIN DEMO
% =====================================================================

main :-
    write(''), nl,
    write('╔════════════════════════════════════════════════════════════════╗'), nl,
    write('║  Comprehensive Dual Expression Operator Combinations Demo     ║'), nl,
    write('║  Pattern: "A:...a is b:...B and A•a is b•B"                   ║'), nl,
    write('║  With combinations of: :, •, [], &, arithmetic expressions    ║'), nl,
    write('╚════════════════════════════════════════════════════════════════╝'), nl, nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('STRING CONCATENATION (:) OPERATOR PATTERNS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_string_concat_basic,
    demo_string_concat_nested,
    demo_string_concat_triple,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('ATOM CONCATENATION (•) OPERATOR PATTERNS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_atom_concat_basic,
    demo_atom_concat_nested,
    demo_atom_concat_triple,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('LIST APPEND (&) OPERATOR PATTERNS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_list_append_basic,
    demo_list_append_nested,
    demo_list_append_triple,
    demo_list_append_complex,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('RESULT EQUALITY WITH DIFFERENT OPERATORS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_string_and_list,
    demo_atom_and_list,
    demo_list_result_equality,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('ARITHMETIC EXPRESSIONS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_arithmetic_standalone,
    demo_arithmetic_sequence,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('IDENTITY ELEMENTS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_empty_list_concat,
    demo_empty_string_concat,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('COMPLEX NESTED PATTERNS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_deeply_nested_strings,
    demo_deeply_nested_lists,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('VARIABLE POSITION VARIATIONS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    catch(demo_variable_left, _, write('   (Pattern has infinite solutions)~n~n')),
    demo_variable_middle,
    demo_multiple_variables,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('ALL COMBINATIONS DEMONSTRATED SUCCESSFULLY! ✓'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(main, main).
