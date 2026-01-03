% demo_find.pl
% Demonstration of the find/2 predicate
% Starlog syntax: Result is find(Template, Goal)
% Equivalent to: findall(Template, (Goal, !), [Result]) with evaluation

:- use_module(starlog).

demo_find :-
    write('==================================================================='), nl,
    write('Demonstration of find/2 Predicate'), nl,
    write('Starlog syntax: Result is find(Template, Goal)'), nl,
    write('==================================================================='), nl, nl,
    
    % Example 1: Basic example from problem statement
    write('Example 1: Result is find(A, starlog_call([A:a] is [a:a]))'), nl,
    write('  This finds A such that [A:a] is [a:a]'), nl,
    starlog_call(Result1 is find(A1, starlog_call([A1:a] is [a:a]))),
    format('  Result: ~w~n', [Result1]),
    format('  Type: ~w~n', [atom]),
    nl,
    
    % Example 2: Finding first member
    write('Example 2: Result is find(X, member(X, [1,2,3]))'), nl,
    write('  This finds the first X from the list'), nl,
    starlog_call(Result2 is find(X, member(X, [1,2,3]))),
    format('  Result: ~w~n', [Result2]),
    nl,
    
    % Example 3: String concatenation
    write('Example 3: Result is find(R, starlog_call(R is "hello":"world"))'), nl,
    write('  This computes string concatenation'), nl,
    starlog_call(Result3 is find(R, starlog_call(R is "hello":"world"))),
    format('  Result: ~w~n', [Result3]),
    nl,
    
    % Example 4: List append
    write('Example 4: Result is find(L, starlog_call(L is [1,2]&[3,4]))'), nl,
    write('  This computes list append'), nl,
    starlog_call(Result4 is find(L, starlog_call(L is [1,2]&[3,4]))),
    format('  Result: ~w~n', [Result4]),
    nl,
    
    % Example 5: Dual expression pattern
    write('Example 5: Result is find(A, starlog_call(([1]&A) is (B&[2])))'), nl,
    write('  This solves for A in the dual expression'), nl,
    starlog_call(Result5 is find(A5, starlog_call(([1]&A5) is (_B&[2])))),
    format('  Result: ~w~n', [Result5]),
    nl,
    
    % Example 6: Demonstrating cut behavior
    write('Example 6: Result is find(X, member(X, [a,b,c,d,e]))'), nl,
    write('  The cut ensures only the FIRST solution is returned'), nl,
    starlog_call(Result6 is find(X6, member(X6, [a,b,c,d,e]))),
    format('  Result: ~w (only first, not all [a,b,c,d,e])~n', [Result6]),
    nl,
    
    % Example 7: Nested list pattern
    write('Example 7: Result is find(A, starlog_call([A•b] is [x•B]))'), nl,
    write('  This finds A in atom concatenation pattern'), nl,
    starlog_call(Result7 is find(A7, starlog_call([A7•b] is [x•_B7]))),
    format('  Result: ~w~n', [Result7]),
    nl,
    
    % Example 8: Arithmetic
    write('Example 8: Result is find(X, X is 10+5)'), nl,
    write('  This evaluates arithmetic'), nl,
    starlog_call(Result8 is find(X8, X8 is 10+5)),
    format('  Result: ~w~n', [Result8]),
    nl,
    
    write('==================================================================='), nl,
    write('All demonstrations complete!'), nl,
    write('==================================================================='), nl.

:- initialization(demo_find, main).
