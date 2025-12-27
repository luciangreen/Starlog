% demo_variable_bound_goals.pl
% Demonstration of variable-bound Starlog goals
% This feature allows you to bind a Starlog goal to a variable and execute it later

:- use_module(starlog).

% Example 1: Basic variable-bound goal with eval inside no_eval
demo_eval_in_no_eval :-
    write('Example 1: Variable-bound goal with eval inside no_eval'), nl,
    write('  Executing: A = (C is no_eval(eval(1+1))), A'), nl,
    A = (C is no_eval(eval(1+1))), A,
    write('  Result: C = '), write(C), nl,
    write('  (Expected: C = 2 - eval forces evaluation inside no_eval)'), nl, nl.

% Example 2: Variable-bound goal with string concatenation
demo_string_concat :-
    write('Example 2: Variable-bound goal with string concatenation'), nl,
    write('  Executing: B = (D is "Hello" : " " : "World"), B'), nl,
    B = (D is "Hello" : " " : "World"), B,
    write('  Result: D = '), write(D), nl, nl.

% Example 3: Variable-bound goal with list operations
demo_list_operations :-
    write('Example 3: Variable-bound goal with list operations'), nl,
    write('  Executing: E = (F is reverse([1,2] & [3,4])), E'), nl,
    E = (F is reverse([1,2] & [3,4])), E,
    write('  Result: F = '), write(F), nl, nl.

% Example 4: Multiple variable-bound goals
demo_multiple_goals :-
    write('Example 4: Multiple variable-bound goals'), nl,
    write('  Executing: G = (H is "first":"part"), G, I = (J is "second":"part"), I'), nl,
    G = (H is "first":"part"), G,
    I = (J is "second":"part"), I,
    write('  Results: H = '), write(H), write(', J = '), write(J), nl, nl.

% Example 5: Variable-bound goal in conditional
demo_conditional :-
    write('Example 5: Using starlog_call for non-immediate execution'), nl,
    K = (L is [1,2] & [3,4]),
    write('  Bound goal: K = (L is [1,2] & [3,4])'), nl,
    write('  Conditional: (starlog_call(K), length(L, N), N > 3)'), nl,
    (starlog_call(K), length(L, N), N > 3 ->
        write('  Result: List has length '), write(N), write(' (> 3)'), nl
    ;
        write('  Result: List is not long enough'), nl
    ),
    nl.

% Example 6: Storing and reusing goals with starlog_call
store_and_execute(Goal) :-
    write('  Executing stored goal with starlog_call...'), nl,
    starlog_call(Goal),
    write('  Done!'), nl.

demo_stored_goals :-
    write('Example 6: Storing and reusing goals with starlog_call'), nl,
    MyGoal = (M is "reusable":"goal"),
    write('  Stored goal: MyGoal = (M is "reusable":"goal")'), nl,
    write('  First execution:'), nl,
    store_and_execute(MyGoal),
    write('  Result: M = '), write(M), nl,
    nl.

% Example 7: Complex nested expression
demo_complex :-
    write('Example 7: Complex nested expression'), nl,
    write('  Executing: N = (O is no_eval("prefix:" : eval("a":"b") : ":suffix")), N'), nl,
    N = (O is no_eval("prefix:" : eval("a":"b") : ":suffix")), N,
    write('  Result: O = '), write(O), nl,
    write('  (Note: only the eval() part is evaluated)'), nl, nl.

% Run all demonstrations
run_demos :-
    write('=============================================='), nl,
    write('Variable-Bound Starlog Goals Demonstration'), nl,
    write('=============================================='), nl, nl,
    demo_eval_in_no_eval,
    demo_string_concat,
    demo_list_operations,
    demo_multiple_goals,
    demo_conditional,
    demo_stored_goals,
    demo_complex,
    write('=============================================='), nl,
    write('Demonstration complete!'), nl,
    write('=============================================='), nl.

:- initialization(run_demos, main).
