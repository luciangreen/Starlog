% demo_operator_selection.pl
% Demonstration of the operator selection pattern: 3 is 1(A is +;-;/)2
% This demonstrates solving "3 is 1 Op 2" where Op is selected from {+, -, /}

:- use_module(starlog_in_prolog).

% Example 1: Find operator where 3 = 1 Op 2
demo_operator_selection_1 :-
    write('Example 1: Find operator Op where 3 = 1 Op 2'), nl,
    write('Testing operators: +, -, /'), nl,
    findall(Op, (member(Op, [+, -, /]), Expr =.. [Op, 1, 2], 3 is Expr), Ops),
    write('Solution: Op = '), write(Ops), nl,
    nl.

% Example 2: Find operator where 6 = 2 Op 3
demo_operator_selection_2 :-
    write('Example 2: Find operator Op where 6 = 2 Op 3'), nl,
    write('Testing operators: +, -, *, /'), nl,
    findall(Op, (member(Op, [+, -, *, /]), Expr =.. [Op, 2, 3], 6 is Expr), Ops),
    write('Solution: Op = '), write(Ops), nl,
    nl.

% Example 3: Find value where 3 = 1 + A
demo_value_selection :-
    write('Example 3: Find value A where 3 = 1 + A'), nl,
    write('Testing values: 1, 2, 3'), nl,
    findall(A, (member(A, [1, 2, 3]), 3 is 1+A), As),
    write('Solution: A = '), write(As), nl,
    nl.

% Example 4: Dual Starlog expression
demo_dual_expression :-
    write('Example 4: Solve ([1] & A) is (B & [2])'), nl,
    ([1] & A) is (B & [2]),
    write('Solution: A = '), write(A), write(', B = '), write(B), nl,
    nl.

% Run all demonstrations
run_all :-
    write('========================================'), nl,
    write('Operator Selection Pattern Demonstrations'), nl,
    write('Problem: "Complete 3 is 1(A is +;-;/)2."'), nl,
    write('========================================'), nl, nl,
    
    demo_operator_selection_1,
    demo_operator_selection_2,
    demo_value_selection,
    demo_dual_expression,
    
    write('========================================'), nl,
    write('All demonstrations complete!'), nl,
    write('========================================'), nl.

:- initialization(run_all, main).
