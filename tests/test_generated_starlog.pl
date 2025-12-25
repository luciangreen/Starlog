% test_generated_starlog.pl
% Test that the generated Starlog code works correctly

:- use_module('../starlog_in_prolog').

% Load the generated Starlog file
:- consult('sample_starlog.pl').

test_generated :-
    write('Testing generated Starlog code...'), nl, nl,
    
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
