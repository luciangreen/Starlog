% test_nested_functions.pl
% Comprehensive tests for nested value-returning functions on LHS and RHS of 'is'

:- use_module('../starlog').

% Helper to print test results
run_test(Name, Goal) :-
    write('Test: '), write(Name), write('... '),
    catch(
        (call(Goal) -> writeln('✓ PASS') ; writeln('✗ FAIL')),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% ============================================================
% Section 1: Nested functions on RHS only (already worked)
% ============================================================

test_nested_rhs_simple :-
    run_test('X is reverse(reverse([1,2,3]))',
             (starlog_call(X is reverse(reverse([1,2,3]))), X = [1,2,3])).

test_nested_rhs_triple :-
    run_test('X is reverse(reverse(reverse([1,2,3])))',
             (starlog_call(X is reverse(reverse(reverse([1,2,3])))), X = [3,2,1])).

test_nested_rhs_different_funcs :-
    run_test('X is length(reverse([1,2,3]))',
             (starlog_call(X is length(reverse([1,2,3]))), X = 3)).

test_nested_rhs_with_concat :-
    run_test('X is reverse([1,2]&[3,4])',
             (starlog_call(X is reverse([1,2]&[3,4])), X = [4,3,2,1])).

test_nested_rhs_concat_then_func :-
    run_test('X is length([1,2]&[3,4])',
             (starlog_call(X is length([1,2]&[3,4])), X = 4)).

% ============================================================
% Section 2: Nested functions on LHS only (NEW FEATURE)
% ============================================================

test_nested_lhs_simple :-
    run_test('reverse(reverse([1,2,3])) is X',
             (starlog_call(reverse(reverse([1,2,3])) is X), X = [1,2,3])).

test_nested_lhs_triple :-
    run_test('reverse(reverse(reverse([1,2,3]))) is X',
             (starlog_call(reverse(reverse(reverse([1,2,3]))) is X), X = [3,2,1])).

test_nested_lhs_different_funcs :-
    run_test('length(reverse([1,2,3])) is X',
             (starlog_call(length(reverse([1,2,3])) is X), X = 3)).

test_nested_lhs_with_concat :-
    run_test('reverse([1,2]&[3,4]) is X',
             (starlog_call(reverse([1,2]&[3,4]) is X), X = [4,3,2,1])).

test_nested_lhs_concat_then_func :-
    run_test('length([1,2]&[3,4]) is X',
             (starlog_call(length([1,2]&[3,4]) is X), X = 4)).

% ============================================================
% Section 3: Nested functions on both LHS and RHS (NEW FEATURE)
% ============================================================

test_nested_both_simple :-
    run_test('reverse(A) is reverse([1,2,3])',
             (starlog_call(reverse(A) is reverse([1,2,3])), A = [1,2,3])).

test_nested_both_different :-
    run_test('reverse(reverse(A)) is reverse([1,2,3])',
             (starlog_call(reverse(reverse(A)) is reverse([1,2,3])), A = [3,2,1])).

test_nested_both_with_variables :-
    run_test('reverse(A) is ([1,2]&[3])',
             (starlog_call(reverse(A) is ([1,2]&[3])), A = [3,2,1])).

% ============================================================
% Section 4: Mixed - concat operators with nested functions
% ============================================================

test_mixed_concat_in_func_rhs :-
    run_test('X is reverse([a]&[b]&[c])',
             (starlog_call(X is reverse([a]&[b]&[c])), X = [c,b,a])).

test_mixed_concat_in_func_lhs :-
    run_test('reverse([a]&[b]&[c]) is X',
             (starlog_call(reverse([a]&[b]&[c]) is X), X = [c,b,a])).

test_mixed_func_in_concat_rhs :-
    run_test('X is (reverse([1,2])&reverse([3,4]))',
             (starlog_call(X is (reverse([1,2])&reverse([3,4]))), X = [2,1,4,3])).

test_mixed_func_in_concat_lhs :-
    run_test('(reverse([1,2])&reverse([3,4])) is X',
             (starlog_call((reverse([1,2])&reverse([3,4])) is X), X = [2,1,4,3])).

test_mixed_string_concat_with_funcs :-
    run_test('(string_length("ab"):string_length("cd")) is X',
             (starlog_call((string_length("ab"):string_length("cd")) is X), X = "22")).

test_mixed_atom_concat_with_funcs :-
    run_test('(atom_string(ab):atom_string(cd)) is X',
             (starlog_call((atom_string(ab):atom_string(cd)) is X), X = "abcd")).

% ============================================================
% Section 5: Deep nesting combinations
% ============================================================

test_deep_nested_rhs :-
    run_test('X is reverse(reverse([1]&[2])&reverse([3]&[4]))',
             (starlog_call(X is reverse(reverse([1]&[2])&reverse([3]&[4]))), X = [3,4,1,2])).

test_deep_nested_lhs :-
    run_test('reverse(reverse([1]&[2])&reverse([3]&[4])) is X',
             (starlog_call(reverse(reverse([1]&[2])&reverse([3]&[4])) is X), X = [3,4,1,2])).

test_deep_nested_both :-
    run_test('reverse(reverse(A)&[5]) is reverse([1,2,3,4]&[5])',
             (starlog_call(reverse(reverse(A)&[5]) is reverse([1,2,3,4]&[5])), A = [4,3,2,1])).

% ============================================================
% Section 6: Edge cases
% ============================================================

test_edge_single_func_lhs :-
    run_test('reverse([]) is X',
             (starlog_call(reverse([]) is X), X = [])).

test_edge_single_func_rhs :-
    run_test('X is reverse([])',
             (starlog_call(X is reverse([])), X = [])).

test_edge_identity_nested :-
    run_test('reverse(reverse([])) is X',
             (starlog_call(reverse(reverse([])) is X), X = [])).

% ============================================================
% Run all tests
% ============================================================

run_tests :-
    writeln(''),
    writeln('========================================================'),
    writeln('  Testing Nested Functions on LHS and RHS of "is"'),
    writeln('========================================================'),
    writeln(''),
    
    writeln('Section 1: Nested functions on RHS only'),
    writeln('--------------------------------------------------'),
    test_nested_rhs_simple,
    test_nested_rhs_triple,
    test_nested_rhs_different_funcs,
    test_nested_rhs_with_concat,
    test_nested_rhs_concat_then_func,
    nl,
    
    writeln('Section 2: Nested functions on LHS only'),
    writeln('--------------------------------------------------'),
    test_nested_lhs_simple,
    test_nested_lhs_triple,
    test_nested_lhs_different_funcs,
    test_nested_lhs_with_concat,
    test_nested_lhs_concat_then_func,
    nl,
    
    writeln('Section 3: Nested functions on both LHS and RHS'),
    writeln('--------------------------------------------------'),
    test_nested_both_simple,
    test_nested_both_different,
    test_nested_both_with_variables,
    nl,
    
    writeln('Section 4: Mixed - concat operators with nested functions'),
    writeln('--------------------------------------------------'),
    test_mixed_concat_in_func_rhs,
    test_mixed_concat_in_func_lhs,
    test_mixed_func_in_concat_rhs,
    test_mixed_func_in_concat_lhs,
    test_mixed_string_concat_with_funcs,
    test_mixed_atom_concat_with_funcs,
    nl,
    
    writeln('Section 5: Deep nesting combinations'),
    writeln('--------------------------------------------------'),
    test_deep_nested_rhs,
    test_deep_nested_lhs,
    test_deep_nested_both,
    nl,
    
    writeln('Section 6: Edge cases'),
    writeln('--------------------------------------------------'),
    test_edge_single_func_lhs,
    test_edge_single_func_rhs,
    test_edge_identity_nested,
    nl,
    
    writeln('========================================================'),
    writeln('  All tests complete!'),
    writeln('========================================================'),
    writeln('').

:- initialization(run_tests, main).
