% test_trace.pl
% Test to understand how dual expressions work

:- use_module(starlog_in_prolog).

test_list_dual :-
    write('Testing ([1] & A) is (B & [2])'), nl,
    ([1] & A) is (B & [2]),
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]),
    write('✓ List test passed'), nl.

test_string_dual :-
    write('Testing ("x" : A) is (B : "y")'), nl,
    ("x" : A) is (B : "y"),
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]),
    write('✓ String test passed'), nl.

main :-
    write('===================================='), nl,
    catch(test_list_dual, E1, (write('✗ List FAILED: '), write(E1), nl)),
    nl,
    catch(test_string_dual, E2, (write('✗ String FAILED: '), write(E2), nl)),
    nl,
    write('===================================='), nl.

:- initialization(main, main).
