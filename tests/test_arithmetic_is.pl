% test_arithmetic_is.pl
% Tests to ensure arithmetic is/2 is preserved

:- use_module('../starlog_in_prolog').

% Test basic arithmetic
test_addition :-
    starlog_call((X is 1+2)),
    X = 3,
    write('✓ Addition test passed'), nl.

% Test multiplication
test_multiplication :-
    starlog_call((X is 3*4)),
    X = 12,
    write('✓ Multiplication test passed'), nl.

% Test complex arithmetic
test_complex_arithmetic :-
    starlog_call((X is (10 + 5) * 2)),
    X = 30,
    write('✓ Complex arithmetic test passed'), nl.

% Test arithmetic with variables
test_arithmetic_vars :-
    A = 5,
    starlog_call((X is A + 10)),
    X = 15,
    write('✓ Arithmetic with variables test passed'), nl.

% Test that Starlog and arithmetic can coexist
test_mixed :-
    starlog_call((S is "hello":"world", L is string_length(S), N is L * 2)),
    N = 20,
    write('✓ Mixed Starlog and arithmetic test passed'), nl.

% Run all tests
run_tests :-
    write('Running arithmetic is/2 preservation tests...'), nl, nl,
    catch(test_addition, E, (write('✗ Addition failed: '), write(E), nl)),
    catch(test_multiplication, E2, (write('✗ Multiplication failed: '), write(E2), nl)),
    catch(test_complex_arithmetic, E3, (write('✗ Complex arithmetic failed: '), write(E3), nl)),
    catch(test_arithmetic_vars, E4, (write('✗ Arithmetic with vars failed: '), write(E4), nl)),
    catch(test_mixed, E5, (write('✗ Mixed test failed: '), write(E5), nl)),
    nl,
    write('Arithmetic tests complete!'), nl.

:- initialization(run_tests, main).
