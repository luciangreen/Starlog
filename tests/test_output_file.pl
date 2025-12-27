% test_output_file.pl
% Tests for starlog_output_file functionality

:- use_module('../starlog').

test_file_output :-
    working_directory(CWD, CWD),
    atom_concat(CWD, 'sample_prolog.pl', FilePath),
    write('=== Testing starlog_output_file ==='), nl, nl,
    starlog_output_file(FilePath),
    nl,
    write('=== Test complete ==='), nl.

:- initialization(test_file_output, main).
