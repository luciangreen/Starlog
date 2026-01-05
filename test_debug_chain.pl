:- use_module(starlog).

% Define test predicate
wrap_a(X, a(X)).

test :-
    writeln('Testing: b(1,c) >> wrap_a'),
    starlog_call(R is b(1,c) >> wrap_a),
    format('Result: ~w~n', [R]).

:- initialization(test, main).
