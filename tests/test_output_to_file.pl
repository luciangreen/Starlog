% test_output_to_file.pl
% Test writing starlog output to a file

:- use_module('../starlog').

test_output_to_file :-
    working_directory(CWD, CWD),
    atom_concat(CWD, 'tests/sample_prolog.pl', InPath),
    atom_concat(CWD, 'tests/sample_starlog.pl', OutPath),
    
    write('Writing Starlog output to sample_starlog.pl...'), nl,
    setup_call_cleanup(
        open(OutPath, write, Stream),
        starlog_output_file(InPath, Stream),
        close(Stream)
    ),
    write('Done! Reading back the file:'), nl, nl,
    setup_call_cleanup(
        open(OutPath, read, ReadStream),
        read_and_display(ReadStream),
        close(ReadStream)
    ),
    delete_file(OutPath).

read_and_display(Stream) :-
    read_string(Stream, _, Content),
    write(Content).

:- initialization(test_output_to_file, main).
