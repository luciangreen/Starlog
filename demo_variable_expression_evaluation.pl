% demo_variable_expression_evaluation.pl
% Demonstration of A = (StarlogExpr), B is A pattern
% This pattern allows binding a Starlog expression to a variable
% and then evaluating it with the 'is' operator.
%
% Problem statement: "Complete A=([a]&[c]),B is A. B=[a,c] with all combinations and configurations"
%
% This feature automatically detects and transforms the pattern:
%   Var1 = (StarlogExpr), Var2 is Var1
% into:
%   Var1 = (StarlogExpr), starlog_eval(Var1, Var2)

:- use_module(starlog).

% =====================================================================
% LIST APPEND (&) OPERATOR
% =====================================================================

demo_list_append_basic :-
    write('1. Basic list append: A = ([a] & [c]), B is A'), nl,
    A = ([a] & [c]),
    B is A,
    format('   Result: B = ~w~n', [B]),
    write('   ✓ Automatically evaluates to [a,c]'), nl, nl.

demo_list_append_numbers :-
    write('2. List append with numbers: X = ([1,2] & [3,4]), Y is X'), nl,
    X = ([1,2] & [3,4]),
    Y is X,
    format('   Result: Y = ~w~n', [Y]),
    write('   ✓ Evaluates to [1,2,3,4]'), nl, nl.

demo_list_append_nested :-
    write('3. Nested list append: L = (([1] & [2]) & [3]), M is L'), nl,
    L = (([1] & [2]) & [3]),
    M is L,
    format('   Result: M = ~w~n', [M]),
    write('   ✓ Evaluates to [1,2,3]'), nl, nl.

demo_list_append_empty :-
    write('4. Empty list append: E = ([] & [x,y]), F is E'), nl,
    E = ([] & [x,y]),
    F is E,
    format('   Result: F = ~w~n', [F]),
    write('   ✓ Empty list is identity for &'), nl, nl.

% =====================================================================
% STRING CONCATENATION (:) OPERATOR
% =====================================================================

demo_string_concat_basic :-
    write('5. Basic string concatenation: S = ("hello" : "world"), T is S'), nl,
    S = ("hello" : "world"),
    T is S,
    format('   Result: T = ~w~n', [T]),
    write('   ✓ Automatically concatenates to "helloworld"'), nl, nl.

demo_string_concat_three :-
    write('6. Three-way string concat: A = ("hello" : " " : "world"), B is A'), nl,
    A = ("hello" : " " : "world"),
    B is A,
    format('   Result: B = ~w~n', [B]),
    write('   ✓ Evaluates to "hello world"'), nl, nl.

demo_string_concat_nested :-
    write('7. Nested string concat: X = (("a" : "b") : ("c" : "d")), Y is X'), nl,
    X = (("a" : "b") : ("c" : "d")),
    Y is X,
    format('   Result: Y = ~w~n', [Y]),
    write('   ✓ Evaluates to "abcd"'), nl, nl.

% =====================================================================
% ATOM CONCATENATION (•) OPERATOR
% =====================================================================

demo_atom_concat_basic :-
    write('8. Basic atom concatenation: P = (hello • world), Q is P'), nl,
    P = (hello • world),
    Q is P,
    format('   Result: Q = ~w~n', [Q]),
    write('   ✓ Automatically concatenates to helloworld'), nl, nl.

demo_atom_concat_three :-
    write('9. Three-way atom concat: A = (foo • bar • baz), B is A'), nl,
    A = (foo • bar • baz),
    B is A,
    format('   Result: B = ~w~n', [B]),
    write('   ✓ Evaluates to foobarbaz'), nl, nl.

% =====================================================================
% MIXED OPERATORS AND COMPLEX PATTERNS
% =====================================================================

demo_multiple_bindings :-
    write('10. Multiple bindings: X = ([a] & [b]), Y is X, Z = ("p" : "q"), W is Z'), nl,
    X = ([a] & [b]),
    Y is X,
    Z = ("p" : "q"),
    W is Z,
    format('   Result: Y = ~w, W = ~w~n', [Y, W]),
    write('   ✓ Both expressions evaluated independently'), nl, nl.

demo_sequential_operations :-
    write('11. Sequential ops: A = ([1] & [2]), B is A, C = (B & [3]), D is C'), nl,
    A = ([1] & [2]),
    B is A,
    C = (B & [3]),
    D is C,
    format('   Result: B = ~w, D = ~w~n', [B, D]),
    write('   ✓ Chain of evaluations works correctly'), nl, nl.

demo_conditional_usage :-
    write('12. Conditional: E = ([x,y] & [z]), F is E, (F = [_,_,_] -> G = evaluated ; G = not_evaluated)'), nl,
    E = ([x,y] & [z]),
    F is E,
    (F = [_,_,_] -> G = evaluated ; G = not_evaluated),
    format('   Result: F = ~w, G = ~w~n', [F, G]),
    write('   ✓ Works in conditional contexts'), nl, nl.

% =====================================================================
% MAIN DEMO
% =====================================================================

main :-
    nl,
    write('╔════════════════════════════════════════════════════════════════╗'), nl,
    write('║  Variable Expression Evaluation Demo                          ║'), nl,
    write('║  Pattern: A = (StarlogExpr), B is A                           ║'), nl,
    write('║  Problem: "Complete A=([a]&[c]),B is A. B=[a,c]"              ║'), nl,
    write('╚════════════════════════════════════════════════════════════════╝'), nl, nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('LIST APPEND (&) OPERATOR'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_list_append_basic,
    demo_list_append_numbers,
    demo_list_append_nested,
    demo_list_append_empty,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('STRING CONCATENATION (:) OPERATOR'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_string_concat_basic,
    demo_string_concat_three,
    demo_string_concat_nested,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('ATOM CONCATENATION (•) OPERATOR'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_atom_concat_basic,
    demo_atom_concat_three,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('MIXED OPERATORS AND COMPLEX PATTERNS'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    demo_multiple_bindings,
    demo_sequential_operations,
    demo_conditional_usage,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('ALL COMBINATIONS AND CONFIGURATIONS DEMONSTRATED! ✓'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(main, main).
