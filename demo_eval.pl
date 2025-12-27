% demo_eval.pl
% Demonstration of the eval feature in Starlog
% This shows how eval is the default behavior and how it can be used
% to force evaluation inside no_eval contexts

:- use_module(starlog).

% Example 1: Default evaluation (eval is implicit)
demo_default_eval :-
    write('Example 1: Default evaluation (eval is implicit)'), nl,
    A is 1+1,
    write('  A is 1+1'), nl,
    write('  Result: A = '), write(A), nl,
    write('  (Default behavior: expressions are evaluated)'), nl, nl.

% Example 2: Explicit eval (same as default)
demo_explicit_eval :-
    write('Example 2: Explicit eval (same as default)'), nl,
    B is eval(1+1),
    write('  B is eval(1+1)'), nl,
    write('  Result: B = '), write(B), nl,
    write('  (Explicit eval, same result as default)'), nl, nl.

% Example 3: eval inside no_eval - forces evaluation
demo_eval_in_no_eval :-
    write('Example 3: eval inside no_eval - forces evaluation'), nl,
    C is no_eval(eval(1+1)),
    write('  C is no_eval(eval(1+1))'), nl,
    write('  Result: C = '), write(C), nl,
    write('  (eval forces evaluation even inside no_eval)'), nl, nl.

% Example 4: Nested eval with Starlog operators
demo_nested_eval_starlog :-
    write('Example 4: Nested eval with Starlog operators'), nl,
    D is no_eval("Result: " : eval("x":"y")),
    write('  D is no_eval("Result: " : eval("x":"y"))'), nl,
    write('  Result: D = '), write(D), nl,
    write('  (Only the eval part is evaluated)'), nl, nl.

% Example 5: Multiple eval in a structure
demo_multiple_eval :-
    write('Example 5: Multiple eval in a structure'), nl,
    E is no_eval([eval(1+1), eval(2+2), 5]),
    write('  E is no_eval([eval(1+1), eval(2+2), 5])'), nl,
    write('  Result: E = '), write(E), nl,
    write('  (Each eval expression is evaluated)'), nl, nl.

% Example 6: eval with list operations
demo_eval_list :-
    write('Example 6: eval with list operations'), nl,
    F is no_eval(eval([1] & [2])),
    write('  F is no_eval(eval([1] & [2]))'), nl,
    write('  Result: F = '), write(F), nl,
    write('  (List append is evaluated)'), nl, nl.

% Example 7: Comparison - no_eval vs eval
demo_comparison :-
    write('Example 7: Comparison - no_eval vs eval'), nl,
    NoEval is no_eval("a":"b"),
    WithEval is no_eval(eval("a":"b")),
    Normal is "a":"b",
    write('  NoEval is no_eval("a":"b")'), nl,
    write('  Result: '), write(NoEval), nl,
    write('  WithEval is no_eval(eval("a":"b"))'), nl,
    write('  Result: '), write(WithEval), nl,
    write('  Normal is "a":"b"'), nl,
    write('  Result: '), write(Normal), nl,
    write('  (Shows the difference between preservation and evaluation)'), nl, nl.

% Example 8: Deeply nested eval
demo_deeply_nested :-
    write('Example 8: Deeply nested eval and no_eval'), nl,
    G is no_eval(no_eval(eval(1+1))),
    write('  G is no_eval(no_eval(eval(1+1)))'), nl,
    write('  Result: G = '), write(G), nl,
    write('  (Inner eval evaluates, outer no_eval preserves the wrapper)'), nl, nl.

% Run all demonstrations
run_demos :-
    write('=============================================='), nl,
    write('Starlog eval Feature Demonstration'), nl,
    write('=============================================='), nl, nl,
    write('IMPORTANT: eval is the DEFAULT behavior in Starlog'), nl,
    write('You only need eval() explicitly to force evaluation'), nl,
    write('inside no_eval contexts.'), nl, nl,
    write('=============================================='), nl, nl,
    demo_default_eval,
    demo_explicit_eval,
    demo_eval_in_no_eval,
    demo_nested_eval_starlog,
    demo_multiple_eval,
    demo_eval_list,
    demo_comparison,
    demo_deeply_nested,
    write('=============================================='), nl,
    write('Demonstration complete!'), nl,
    write('=============================================='), nl.

:- initialization(run_demos, main).
