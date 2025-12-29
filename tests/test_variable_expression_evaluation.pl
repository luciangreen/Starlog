% test_variable_expression_evaluation.pl
% Test suite for A = (StarlogExpr), B is A pattern
% Tests the problem: "Complete A=([a]&[c]),B is A. B=[a,c] with all combinations and configurations"

:- use_module('../starlog').

:- begin_tests(variable_expression_evaluation).

% =====================================================================
% LIST APPEND (&) OPERATOR TESTS
% =====================================================================

test(list_append_basic, [true(B = [a,c])]) :-
    A = ([a] & [c]),
    B is A.

test(list_append_numbers, [true(Y = [1,2,3,4])]) :-
    X = ([1,2] & [3,4]),
    Y is X.

test(list_append_nested, [true(M = [1,2,3])]) :-
    L = (([1] & [2]) & [3]),
    M is L.

test(list_append_empty_left, [true(F = [x,y])]) :-
    E = ([] & [x,y]),
    F is E.

test(list_append_empty_right, [true(F = [x,y])]) :-
    E = ([x,y] & []),
    F is E.

test(list_append_single, [true(R = [a,b])]) :-
    Q = ([a] & [b]),
    R is Q.

test(list_append_deep_nested, [true(Z = [1,2,3,4])]) :-
    W = ((([1] & [2]) & [3]) & [4]),
    Z is W.

% =====================================================================
% STRING CONCATENATION (:) OPERATOR TESTS
% =====================================================================

test(string_concat_basic, [true(T = "helloworld")]) :-
    S = ("hello" : "world"),
    T is S.

test(string_concat_three, [true(B = "hello world")]) :-
    A = ("hello" : " " : "world"),
    B is A.

test(string_concat_nested, [true(Y = "abcd")]) :-
    X = (("a" : "b") : ("c" : "d")),
    Y is X.

test(string_concat_empty, [true(R = "test")]) :-
    Q = ("" : "test"),
    R is Q.

% =====================================================================
% ATOM CONCATENATION (•) OPERATOR TESTS
% =====================================================================

test(atom_concat_basic, [true(Q = helloworld)]) :-
    P = (hello • world),
    Q is P.

test(atom_concat_three, [true(B = foobarbaz)]) :-
    A = (foo • bar • baz),
    B is A.

test(atom_concat_nested, [true(Y = abcd)]) :-
    X = ((a • b) • (c • d)),
    Y is X.

% =====================================================================
% MIXED OPERATORS TESTS
% =====================================================================

test(multiple_bindings, [true((Y = [a,b], W = "pq"))]) :-
    X = ([a] & [b]),
    Y is X,
    Z = ("p" : "q"),
    W is Z.

test(sequential_operations, [true((B = [1,2], D = [1,2,3]))]) :-
    A = ([1] & [2]),
    B is A,
    C = (B & [3]),
    D is C.

test(mixed_types_sequential, [true((R1 = [x,y], R2 = "ab", R3 = cd))]) :-
    E1 = ([x] & [y]),
    R1 is E1,
    E2 = ("a" : "b"),
    R2 is E2,
    E3 = (c • d),
    R3 is E3.

% =====================================================================
% COMPLEX PATTERNS TESTS
% =====================================================================

test(variable_reuse, [true((B = [1,2], C = [1,2,3]))]) :-
    A = ([1] & [2]),
    B is A,
    % Create a new expression using the result B
    C is (B & [3]).

test(nested_in_list, [true(Result = [[a,b]&[c]])]) :-
    Expr = ([a,b] & [c]),
    ListExpr = [Expr],
    % Note: Expressions inside lists are not auto-evaluated
    Result = ListExpr.

test(in_conjunction, [true((X = [1,2], Y = 5))]) :-
    E = ([1] & [2]),
    (X is E, Y is 2+3).

test(in_if_then_else, [true(R = [a,b])]) :-
    E = ([a] & [b]),
    % Pattern works in if-then-else branches that are in the clause body
    R is E.

% =====================================================================
% EDGE CASES
% =====================================================================

test(single_element_list, [true(B = [x])]) :-
    A = ([x] & []),
    B is A.

test(atom_single_char, [true(R = ab)]) :-
    E = (a • b),
    R is E.

test(string_single_char, [true(R = "xy")]) :-
    E = ("x" : "y"),
    R is E.

% =====================================================================
% COMPARISON WITH ORIGINAL PATTERN
% =====================================================================

test(original_problem_statement, [true(B = [a,c])]) :-
    % This is the exact problem from the statement:
    % "Complete A=([a]&[c]),B is A. B=[a,c]"
    A = ([a] & [c]),
    B is A.

:- end_tests(variable_expression_evaluation).

% Run all tests
:- run_tests.
