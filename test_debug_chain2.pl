:- use_module(starlog).

% Define test predicates
wrap_a(X, a(X)).
wrap_d(X, d(X)).

test :-
    writeln('Testing: b(1,c) >> wrap_a >> wrap_d'),
    starlog_call(R is b(1,c) >> wrap_a >> wrap_d),
    format('Result: ~w~n', [R]).

:- initialization(test, main).
