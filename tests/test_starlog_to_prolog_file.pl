% test_starlog_to_prolog_file.pl
% Tests for starlog_to_prolog_file functionality

:- use_module('../starlog').

test_file_conversion :-
    write('=== Testing Starlog to Prolog File Conversion ==='), nl, nl,
    write('Converting sample_starlog.pl...'), nl, nl,
    setup_call_cleanup(
        open('tests/sample_starlog.pl', write, Stream),
        starlog_output_file('tests/sample_prolog.pl', Stream),
        close(Stream)
    ),
    setup_call_cleanup(
        true,
        starlog_to_prolog_file('tests/sample_starlog.pl'),
        delete_file('tests/sample_starlog.pl')
    ).

:- initialization(test_file_conversion, main).
