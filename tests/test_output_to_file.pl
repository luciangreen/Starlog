% test_output_to_file.pl
% Test writing starlog output to a file

:- use_module('../starlog_in_prolog').

test_output_to_file :-
    write('Writing Starlog output to sample_starlog.pl...'), nl,
    setup_call_cleanup(
        open('sample_starlog.pl', write, Stream),
        starlog_output_file('sample_prolog.pl', Stream),
        close(Stream)
    ),
    write('Done! Reading back the file:'), nl, nl,
    setup_call_cleanup(
        open('sample_starlog.pl', read, ReadStream),
        read_and_display(ReadStream),
        close(ReadStream)
    ).

read_and_display(Stream) :-
    read_string(Stream, _, Content),
    write(Content).

:- initialization(test_output_to_file, main).
