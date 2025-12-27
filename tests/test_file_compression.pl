% test_file_compression.pl
% Tests for file-level Starlog compression

:- use_module('../starlog').

% Test 1: File output with compression
test_file_compression :-
    write('Test 1: File output with compression'), nl,
    % Create temporary output
    with_output_to(string(Output),
        starlog_output_file('sample_for_compression.pl', current_output, [compress(true)])
    ),
    % Check that greet is compressed
    (sub_string(Output, _, _, _, 'C is "Hello, ":A:" ":B') ->
        write('✓ Greet predicate compressed correctly'), nl
    ;
        write('✗ Greet predicate compression failed'), nl,
        write('Expected substring: C is "Hello, ":A:" ":B'), nl,
        write('Got: '), nl, write(Output), nl
    ),
    % Check that combine_and_reverse is compressed
    (sub_string(Output, _, _, _, 'C is reverse(A&B)') ->
        write('✓ Combine_and_reverse predicate compressed correctly'), nl
    ;
        write('✗ Combine_and_reverse predicate compression failed'), nl
    ),
    % Check that process_strings is compressed
    (sub_string(Output, _, _, _, 'D is result•string_length(A:B:C)') ->
        write('✓ Process_strings predicate compressed correctly'), nl
    ;
        write('✗ Process_strings predicate compression failed'), nl
    ).

% Test 2: File output without compression
test_file_no_compression :-
    write('Test 2: File output without compression'), nl,
    with_output_to(string(Output),
        starlog_output_file('sample_for_compression.pl', current_output, [compress(false)])
    ),
    % Check that greet has intermediate variables
    (sub_string(Output, _, _, _, 'D is "Hello, ":A') ->
        write('✓ Greet predicate not compressed (has intermediate vars)'), nl
    ;
        write('✗ Greet predicate should have intermediate variables'), nl,
        write('Got: '), nl, write(Output), nl
    ).

% Test 3: Default behavior (no compression)
test_file_default :-
    write('Test 3: Default behavior (no compression)'), nl,
    with_output_to(string(Output),
        starlog_output_file('sample_for_compression.pl', current_output)
    ),
    % Check that greet has intermediate variables by default
    (sub_string(Output, _, _, _, 'D is "Hello, ":A') ->
        write('✓ Default behavior is no compression'), nl
    ;
        write('✗ Default should not compress'), nl
    ).

% Run all tests
run_tests :-
    write('=== Running File Compression Tests ==='), nl, nl,
    catch(test_file_compression, E1, (write('✗ Test 1 exception: '), write(E1), nl)),
    nl,
    catch(test_file_no_compression, E2, (write('✗ Test 2 exception: '), write(E2), nl)),
    nl,
    catch(test_file_default, E3, (write('✗ Test 3 exception: '), write(E3), nl)),
    nl,
    write('=== File compression tests complete ==='), nl.

:- initialization(run_tests, main).
