% demo_output_eval_options.pl
% Demonstration of the eval/no_eval output control options

:- use_module(starlog).

demo_default_behavior :-
    write('=== Default Behavior (strips both eval and no_eval) ==='), nl, nl,
    write('Input: A is no_eval(1+1)'), nl,
    write('Output: '),
    starlog_output_code(A is no_eval(1+1)),
    nl,
    write('Input: B is eval("x":"y")'), nl,
    write('Output: '),
    starlog_output_code(B is eval("x":"y")),
    nl, nl.

demo_keep_no_eval :-
    write('=== Keep no_eval() wrappers (output_no_eval(true)) ==='), nl, nl,
    write('Input: A is no_eval(1+1)'), nl,
    write('Output: '),
    starlog_output_code(A is no_eval(1+1), _, [output_no_eval(true)]),
    nl,
    write('Input: B is eval("x":"y")'), nl,
    write('Output: '),
    starlog_output_code(B is eval("x":"y"), _, [output_no_eval(true)]),
    nl, nl.

demo_keep_eval :-
    write('=== Keep eval() wrappers (output_eval(true)) ==='), nl, nl,
    write('Input: A is eval("x":"y")'), nl,
    write('Output: '),
    starlog_output_code(A is eval("x":"y"), _, [output_eval(true)]),
    nl,
    write('Input: B is no_eval(1+1)'), nl,
    write('Output: '),
    starlog_output_code(B is no_eval(1+1), _, [output_eval(true)]),
    nl, nl.

demo_keep_both :-
    write('=== Keep both wrappers (output_eval(true), output_no_eval(true)) ==='), nl, nl,
    write('Input: A is no_eval(eval(1+1))'), nl,
    write('Output: '),
    starlog_output_code(A is no_eval(eval(1+1)), _, [output_eval(true), output_no_eval(true)]),
    nl,
    write('Input: B is no_eval("x" : eval("y":"z"))'), nl,
    write('Output: '),
    starlog_output_code(B is no_eval("x" : eval("y":"z")), _, [output_eval(true), output_no_eval(true)]),
    nl, nl.

demo_comparison :-
    write('=== Comparison of All Options ==='), nl, nl,
    write('Expression: A is no_eval("hello" : eval("world"))'), nl, nl,
    write('Default (strip both):'), nl,
    write('  '),
    starlog_output_code(A is no_eval("hello" : eval("world"))),
    nl,
    write('Keep no_eval only:'), nl,
    write('  '),
    starlog_output_code(B is no_eval("hello" : eval("world")), _, [output_no_eval(true)]),
    nl,
    write('Keep eval only:'), nl,
    write('  '),
    starlog_output_code(C is no_eval("hello" : eval("world")), _, [output_eval(true)]),
    nl,
    write('Keep both:'), nl,
    write('  '),
    starlog_output_code(D is no_eval("hello" : eval("world")), _, [output_eval(true), output_no_eval(true)]),
    nl, nl.

run_demos :-
    write('=================================================='), nl,
    write('Starlog Output: eval/no_eval Control Options Demo'), nl,
    write('=================================================='), nl, nl,
    demo_default_behavior,
    demo_keep_no_eval,
    demo_keep_eval,
    demo_keep_both,
    demo_comparison,
    write('=================================================='), nl,
    write('Demo complete!'), nl,
    write('=================================================='), nl.

:- initialization(run_demos, main).
