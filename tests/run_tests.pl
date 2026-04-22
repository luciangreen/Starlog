:- use_module(pr2_stage1_failing_tests).
:- use_module(pr2_stage3_tests).
:- use_module(pr2_stage4_tests).
:- use_module(pr2_stage5_tests).
:- use_module(pr2_stage6_tests).
:- use_module(pr2_stage7_tests).
:- use_module(pr2_stage8_tests).
:- use_module(pr2_stage9_tests).
:- use_module(pr2_stage10_tests).
:- use_module(pr2_stage11_tests).
:- use_module(pr2_stage12_tests).
:- use_module(pr2_stage13_tests).
:- use_module(pr2_stage14_tests).
:- use_module(pr3_stage6_tests).

:- initialization(main, main).

main :-
    run_pr2_stage1_tests,
    run_pr2_stage3_tests,
    run_pr2_stage4_tests,
    run_pr2_stage5_tests,
    run_pr2_stage6_tests,
    run_pr2_stage7_tests,
    run_pr2_stage8_tests,
    run_pr2_stage9_tests,
    run_pr2_stage10_tests,
    run_pr2_stage11_tests,
    run_pr2_stage12_tests,
    run_pr2_stage13_tests,
    run_pr2_stage14_tests,
    run_pr3_stage6_tests.
