% demo_dual_expressions.pl
% Demonstration of the new dual Starlog expression is/2 feature
% This file shows practical examples of using dual expressions

:- use_module(starlog_in_prolog).

% Demo 1: Solving list equations
demo_list_equations :-
    write('Demo 1: Solving List Append Equations'), nl,
    write('========================================'), nl,
    write('Problem: Find A and B such that [1]&A equals B&[2]'), nl,
    ([1] & A) is (B & [2]),
    write('Solution: A = '), write(A), write(', B = '), write(B), nl,
    nl.

% Demo 2: Verifying expression equivalence
demo_equivalence :-
    write('Demo 2: Verifying Expression Equivalence'), nl,
    write('=========================================='), nl,
    write('Checking if ("hello" : " ") is equivalent to ("hello" : " ")'), nl,
    (("hello" : " ") is ("hello" : " ") ->
        write('✓ Expressions are equivalent!') ; write('✗ Not equivalent')),
    nl, nl.

% Demo 3: Complex nested patterns
demo_nested :-
    write('Demo 3: Complex Nested Patterns'), nl,
    write('================================='), nl,
    write('Problem: Solve (([1] & [2]) & A) is (B & [3])'), nl,
    (([1] & [2]) & A) is (B & [3]),
    write('Solution: A = '), write(A), write(', B = '), write(B), nl,
    nl.

% Demo 4: Finding operators
demo_operators :-
    write('Demo 4: Finding Operators'), nl,
    write('==========================='), nl,
    write('Problem: Find operator Op such that 3 = 1 Op 2'), nl,
    findall(Op, (
        member(Op, [+, -, *, /]),
        Expr =.. [Op, 1, 2],
        3 is Expr
    ), Ops),
    write('Solution: Op = '), write(Ops), nl,
    nl.

% Demo 5: Finding values
demo_values :-
    write('Demo 5: Finding Values'), nl,
    write('========================'), nl,
    write('Problem: Find A from {1,2,3} such that 3 = 1+A'), nl,
    findall(A, (
        member(A, [1, 2, 3]),
        3 is 1+A
    ), As),
    write('Solution: A = '), write(As), nl,
    nl.

% Demo 6: Practical example - string manipulation
demo_practical :-
    write('Demo 6: Practical String Manipulation'), nl,
    write('======================================='), nl,
    write('Building strings from parts:'), nl,
    
    % Build a greeting using Starlog
    FirstName = "John",
    LastName = "Doe",
    FullName is FirstName : " " : LastName,
    Greeting is "Hello, " : FullName : "!",
    
    write('  FirstName = '), write(FirstName), nl,
    write('  LastName = '), write(LastName), nl,
    write('  FullName = '), write(FullName), nl,
    write('  Greeting = '), write(Greeting), nl,
    nl.

% Demo 7: List manipulation chains
demo_list_chains :-
    write('Demo 7: List Manipulation Chains'), nl,
    write('=================================='), nl,
    write('Combining multiple lists:'), nl,
    
    List1 is [1, 2] & [3, 4],
    List2 is List1 & [5, 6],
    
    write('  [1,2] & [3,4] = '), write(List1), nl,
    write('  Result & [5,6] = '), write(List2), nl,
    nl.

% Run all demos
run_all_demos :-
    write('======================================================='), nl,
    write('Dual Starlog Expression is/2 Feature Demonstrations'), nl,
    write('======================================================='), nl,
    nl,
    demo_list_equations,
    demo_equivalence,
    demo_nested,
    demo_operators,
    demo_values,
    demo_practical,
    demo_list_chains,
    write('======================================================='), nl,
    write('All demonstrations complete!'), nl,
    write('======================================================='), nl.

:- initialization(run_all_demos, main).
