% test_generated_starlog.pl
% Test that the generated Starlog code works correctly

:- use_module('../starlog').

test_generated :-
    write('Testing generated Starlog code...'), nl, nl,
    
    % First generate the starlog file
    working_directory(CWD, CWD),
    atom_concat(CWD, 'sample_prolog.pl', InPath),
    atom_concat(CWD, 'sample_starlog.pl', OutPath),
    
    setup_call_cleanup(
        open(OutPath, write, Stream),
        starlog_output_file(InPath, Stream),
        close(Stream)
    ),
    
    % Load the generated Starlog file
    consult(OutPath),
    
    write('Test 1: greet/3'), nl,
    greet("John", "Doe", Greeting),
    format('  Result: ~w~n', [Greeting]),
    (Greeting = "Hello, John Doe" -> 
        write('  ✓ PASSED') ; 
        write('  ✗ FAILED')
    ), nl, nl,
    
    write('Test 2: combine_lists/4'), nl,
    combine_lists([1,2], [3,4], Combined, Reversed),
    format('  Combined: ~w~n', [Combined]),
    format('  Reversed: ~w~n', [Reversed]),
    (Combined = [1,2,3,4], Reversed = [4,3,2,1] -> 
        write('  ✓ PASSED') ; 
        write('  ✗ FAILED')
    ), nl, nl,
    
    write('All tests complete!'), nl.

:- initialization(test_generated, main).
