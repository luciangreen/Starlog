% test_starlog_to_prolog_file.pl
% Tests for starlog_to_prolog_file functionality

:- use_module('../starlog').

test_file_conversion :-
    write('=== Testing Starlog to Prolog File Conversion ==='), nl, nl,
    write('Converting sample_starlog.pl...'), nl, nl,
    starlog_to_prolog_file('sample_starlog.pl').

:- initialization(test_file_conversion, main).
