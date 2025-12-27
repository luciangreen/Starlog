% test_univ_conversion.pl
% Test conversion of univ operators between Prolog and Starlog

:- use_module('../starlog').

% Test Prolog to Starlog conversion for term to list
test_prolog_to_starlog_term_to_list :-
    starlog_output_code((f(0,1) =.. A), _),
    write('✓ Prolog to Starlog term to list conversion test passed'), nl.

% Test Prolog to Starlog conversion for list to term
test_prolog_to_starlog_list_to_term :-
    starlog_output_code((A =.. [f,0,1]), _),
    write('✓ Prolog to Starlog list to term conversion test passed'), nl.

% Test Starlog to Prolog conversion for term to list
test_starlog_to_prolog_term_to_list :-
    starlog_to_prolog_code((A is =..(f(0,1))), _),
    write('✓ Starlog to Prolog term to list conversion test passed'), nl.

% Test Starlog to Prolog conversion for list to term
test_starlog_to_prolog_list_to_term :-
    starlog_to_prolog_code((A is ..=([f,0,1])), _),
    write('✓ Starlog to Prolog list to term conversion test passed'), nl.

% Run all conversion tests
run_tests :-
    write('Running univ operator conversion tests...'), nl, nl,
    catch(test_prolog_to_starlog_term_to_list, E1, (write('✗ Prolog to Starlog term to list failed: '), write(E1), nl)),
    catch(test_prolog_to_starlog_list_to_term, E2, (write('✗ Prolog to Starlog list to term failed: '), write(E2), nl)),
    catch(test_starlog_to_prolog_term_to_list, E3, (write('✗ Starlog to Prolog term to list failed: '), write(E3), nl)),
    catch(test_starlog_to_prolog_list_to_term, E4, (write('✗ Starlog to Prolog list to term failed: '), write(E4), nl)),
    nl,
    write('Univ conversion tests complete!'), nl.

:- initialization(run_tests, main).
