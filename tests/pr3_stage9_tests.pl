:- module(pr3_stage9_tests, [run_pr3_stage9_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr3_stage9_tests :-
    run_tests([pr3_stage9_random_permutation]).

:- begin_tests(pr3_stage9_random_permutation).

% ── Basic usage ──────────────────────────────────────────────────────────────

test(random_permutation_integer_list_is_permutation) :-
    starlog_call(A is random_permutation([1,2,3])),
    msort(A, Sorted),
    assertion(Sorted == [1,2,3]).

test(random_permutation_integer_list_correct_length) :-
    starlog_call(A is random_permutation([4,5,6,7])),
    length(A, Len),
    assertion(Len == 4).

test(random_permutation_atom_list_is_permutation) :-
    starlog_call(A is random_permutation([a,b,c])),
    msort(A, Sorted),
    assertion(Sorted == [a,b,c]).

test(random_permutation_string_list_is_permutation) :-
    starlog_call(A is random_permutation(["x","y","z"])),
    msort(A, Sorted),
    assertion(Sorted == ["x","y","z"]).

test(random_permutation_empty_list) :-
    starlog_call(A is random_permutation([])),
    assertion(A == []).

test(random_permutation_singleton_list) :-
    starlog_call(A is random_permutation([42])),
    assertion(A == [42]).

% ── Nested in predicate calls ─────────────────────────────────────────────

test(random_permutation_nested_in_nth1) :-
    starlog_call(A is nth1(2, random_permutation([1,2,3]))),
    assertion(member(A, [1,2,3])).

test(random_permutation_nested_in_nth0) :-
    starlog_call(A is nth0(0, random_permutation([10,20,30]))),
    assertion(member(A, [10,20,30])).

test(random_permutation_nested_in_length) :-
    starlog_call(A is length(random_permutation([a,b,c,d]))),
    assertion(A == 4).

test(random_permutation_nested_in_sort) :-
    starlog_call(A is sort(random_permutation([3,1,2]))),
    assertion(A == [1,2,3]).

test(random_permutation_nested_in_msort) :-
    starlog_call(A is msort(random_permutation([3,1,2,1]))),
    assertion(A == [1,1,2,3]).

test(random_permutation_nested_in_reverse) :-
    starlog_call(A is reverse(random_permutation([1,2,3]))),
    msort(A, Sorted),
    assertion(Sorted == [1,2,3]).

test(random_permutation_nested_in_last) :-
    starlog_call(A is last(random_permutation([a,b,c]))),
    assertion(member(A, [a,b,c])).

test(random_permutation_nested_in_min_list) :-
    starlog_call(A is min_list(random_permutation([5,3,8,1]))),
    assertion(A == 1).

test(random_permutation_nested_in_max_list) :-
    starlog_call(A is max_list(random_permutation([5,3,8,1]))),
    assertion(A == 8).

test(random_permutation_nested_in_sum_list) :-
    starlog_call(A is sum_list(random_permutation([1,2,3,4]))),
    assertion(A == 10).

% ── Nested random_permutation inside random_permutation ──────────────────────

test(random_permutation_double_nested_is_permutation) :-
    starlog_call(A is random_permutation(random_permutation([1,2,3]))),
    msort(A, Sorted),
    assertion(Sorted == [1,2,3]).

% ── Method chaining ──────────────────────────────────────────────────────────

test(random_permutation_method_chain_sort) :-
    starlog_call(A is random_permutation([3,1,2]) >> sort),
    assertion(A == [1,2,3]).

test(random_permutation_method_chain_reverse) :-
    starlog_call(A is random_permutation([1,2,3]) >> reverse),
    msort(A, Sorted),
    assertion(Sorted == [1,2,3]).

test(random_permutation_method_chain_length) :-
    starlog_call(A is random_permutation([a,b,c,d]) >> length),
    assertion(A == 4).

test(random_permutation_method_chain_sort_then_reverse) :-
    starlog_call(A is random_permutation([3,1,2]) >> sort >> reverse),
    assertion(A == [3,2,1]).

test(random_permutation_method_chain_append) :-
    starlog_call(A is random_permutation([1,2]) >> append([3,4])),
    msort(A, Sorted),
    assertion(Sorted == [1,2,3,4]).

% ── Combinations with list operators ─────────────────────────────────────────

test(random_permutation_nested_in_append_concat) :-
    starlog_call(A is random_permutation([1,2]) & [3]),
    length(A, Len),
    assertion(Len == 3).

test(random_permutation_used_as_arg_in_append_concat) :-
    starlog_call(A is [0] & random_permutation([1,2,3])),
    length(A, Len),
    assertion(Len == 4).

% ── LHS of is ────────────────────────────────────────────────────────────────

test(random_permutation_on_lhs_of_is) :-
    starlog_call(random_permutation([1,2,3]) is X),
    msort(X, Sorted),
    assertion(Sorted == [1,2,3]).

% ── Different types ───────────────────────────────────────────────────────────

test(random_permutation_mixed_type_list) :-
    starlog_call(A is random_permutation([1, a, "hello"])),
    length(A, Len),
    assertion(Len == 3).

test(random_permutation_nested_types_nth1_atom_list) :-
    starlog_call(A is nth1(1, random_permutation([x,y,z]))),
    assertion(member(A, [x,y,z])).

test(random_permutation_nested_types_sort_integer_list) :-
    starlog_call(A is sort(random_permutation([5,1,3,2,4]))),
    assertion(A == [1,2,3,4,5]).

:- end_tests(pr3_stage9_random_permutation).
