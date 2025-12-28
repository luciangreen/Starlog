% demo_find.pl
% Demonstration of the find/3 predicate
% Requirement: find(A,B,C) :- findall(A,(B,!),[C]).

:- use_module(starlog).

demo_find :-
    write('==================================================================='), nl,
    write('Demonstration of find/3 Predicate'), nl,
    write('Requirement: find(A,B,C) :- findall(A,(B,!),[C]).'), nl,
    write('==================================================================='), nl, nl,
    
    % Example 1: Basic example from problem statement
    write('Example 1: find(A, [A:a] is [a:a], Result)'), nl,
    write('  This finds A such that [A:a] is [a:a]'), nl,
    find(A1, starlog_call([A1:a] is [a:a]), Result1),
    format('  Result: ~w~n', [Result1]),
    format('  Type: ~w~n', [atom]),
    nl,
    
    % Example 2: Finding first member
    write('Example 2: find(X, member(X, [1,2,3]), Result)'), nl,
    write('  This finds the first X from the list'), nl,
    find(X, member(X, [1,2,3]), Result2),
    format('  Result: ~w~n', [Result2]),
    nl,
    
    % Example 3: String concatenation
    write('Example 3: find(R, R is "hello":"world", Result)'), nl,
    write('  This computes string concatenation'), nl,
    find(R, starlog_call(R is "hello":"world"), Result3),
    format('  Result: ~w~n', [Result3]),
    nl,
    
    % Example 4: List append
    write('Example 4: find(L, L is [1,2]&[3,4], Result)'), nl,
    write('  This computes list append'), nl,
    find(L, starlog_call(L is [1,2]&[3,4]), Result4),
    format('  Result: ~w~n', [Result4]),
    nl,
    
    % Example 5: Dual expression pattern
    write('Example 5: find(A, ([1]&A) is (B&[2]), Result)'), nl,
    write('  This solves for A in the dual expression'), nl,
    find(A5, starlog_call(([1]&A5) is (_B&[2])), Result5),
    format('  Result: ~w~n', [Result5]),
    nl,
    
    % Example 6: Demonstrating cut behavior
    write('Example 6: find(X, member(X, [a,b,c,d,e]), Result)'), nl,
    write('  The cut ensures only the FIRST solution is returned'), nl,
    find(X6, member(X6, [a,b,c,d,e]), Result6),
    format('  Result: ~w (only first, not all [a,b,c,d,e])~n', [Result6]),
    nl,
    
    % Example 7: Nested list pattern
    write('Example 7: find(A, [A•b] is [x•B], Result)'), nl,
    write('  This finds A in atom concatenation pattern'), nl,
    find(A7, starlog_call([A7•b] is [x•_B7]), Result7),
    format('  Result: ~w~n', [Result7]),
    nl,
    
    % Example 8: Arithmetic
    write('Example 8: find(X, X is 10+5, Result)'), nl,
    write('  This evaluates arithmetic'), nl,
    find(X8, X8 is 10+5, Result8),
    format('  Result: ~w~n', [Result8]),
    nl,
    
    write('==================================================================='), nl,
    write('All demonstrations complete!'), nl,
    write('==================================================================='), nl.

:- initialization(demo_find, main).
