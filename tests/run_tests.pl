:- use_module(pr2_stage1_failing_tests).
:- use_module(pr2_stage3_tests).
:- use_module(pr2_stage4_tests).

:- initialization(main, main).

main :-
    run_pr2_stage1_tests,
    run_pr2_stage3_tests,
    run_pr2_stage4_tests.
