% test_output_file.pl
% Tests for starlog_output_file functionality

:- use_module('../starlog_in_prolog').

test_file_output :-
    write('=== Testing starlog_output_file ==='), nl, nl,
    starlog_output_file('sample_prolog.pl'),
    nl,
    write('=== Test complete ==='), nl.

:- initialization(test_file_output, main).
