% test_problem.pl
% Test for the specific problem statement: a:A is B:b and a•A is B•b

:- use_module(starlog_in_prolog).

test_string_concat :-
    write('Testing a:A is B:b...'), nl,
    (a:A) is (B:b),
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]),
    write('✓ Test passed'), nl.

test_atom_concat :-
    write('Testing a•A is B•b...'), nl,
    (a•A) is (B•b),
    format('  A = ~w~n', [A]),
    format('  B = ~w~n', [B]),
    write('✓ Test passed'), nl.

main :-
    write('===================================='), nl,
    write('Testing Problem Statement'), nl,
    write('Complete a:A is B:b and a•A is B•b'), nl,
    write('===================================='), nl, nl,
    catch(test_string_concat, E1, (write('✗ FAILED: '), write(E1), nl)),
    nl,
    catch(test_atom_concat, E2, (write('✗ FAILED: '), write(E2), nl)),
    nl,
    write('===================================='), nl.

:- initialization(main, main).
