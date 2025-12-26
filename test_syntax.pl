:- use_module(starlog_in_prolog).

% Test if we can write: ([1] & A) is (B & [2])
test_problematic_pattern :-
    write('Testing: ([1] & A) is (B & [2])'), nl,
    % Try it
    ([1] & A) is (B & [2]),
    write('A = '), write(A), nl,
    write('B = '), write(B), nl.

:- initialization(test_problematic_pattern, main).
