% demo_no_eval.pl
% Demonstration of the no_eval feature in Starlog

:- use_module(starlog_in_prolog).

% Example 1: Preserving arithmetic expressions
demo_arithmetic :-
    write('Example 1: Preserving arithmetic expressions'), nl,
    A is no_eval(1+1),
    write('  A is no_eval(1+1)'), nl,
    write('  Result: A = '), write(A), nl,
    write('  (Expected: 1+1, not 2)'), nl, nl.

% Example 2: Preserving Starlog operators
demo_starlog_operators :-
    write('Example 2: Preserving Starlog operators'), nl,
    B is no_eval("hello":"world"),
    write('  B is no_eval("hello":"world")'), nl,
    write('  Result: B = '), write(B), nl,
    write('  (Expected: "hello":"world", not "helloworld")'), nl, nl.

% Example 3: Comparing normal evaluation vs no_eval
demo_comparison :-
    write('Example 3: Comparing normal evaluation vs no_eval'), nl,
    Normal is 2 * 3,
    NoEval is no_eval(2 * 3),
    write('  Normal is 2 * 3'), nl,
    write('  Result: '), write(Normal), nl,
    write('  NoEval is no_eval(2 * 3)'), nl,
    write('  Result: '), write(NoEval), nl, nl.

% Example 4: Using no_eval with complex expressions
demo_complex :-
    write('Example 4: Using no_eval with complex expressions'), nl,
    C is no_eval((1+2)*(3+4)),
    write('  C is no_eval((1+2)*(3+4))'), nl,
    write('  Result: C = '), write(C), nl,
    write('  (Expression preserved, not evaluated to 21)'), nl, nl.

% Example 5: Practical use case - storing formulas
store_formula(Name, Formula) :-
    write('Storing formula "'), write(Name), write('": '),
    write(Formula), nl.

demo_formula_storage :-
    write('Example 5: Storing formulas as data'), nl,
    F is no_eval(x*2 + y),
    store_formula('double_x_plus_y', F),
    nl.

% Run all demonstrations
run_demos :-
    write('=============================================='), nl,
    write('Starlog no_eval Feature Demonstration'), nl,
    write('=============================================='), nl, nl,
    demo_arithmetic,
    demo_starlog_operators,
    demo_comparison,
    demo_complex,
    demo_formula_storage,
    write('=============================================='), nl,
    write('Demonstration complete!'), nl,
    write('=============================================='), nl.

:- initialization(run_demos, main).
