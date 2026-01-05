% Test understanding the problem
:- use_module(starlog).

% Example from problem statement
% d(a(b(1,C))) should be callable as b(1,C).a.d

% Let's define some simple test predicates
a(X, a(X)).
b(X, Y, b(X,Y)).
d(X, d(X)).

test1 :-
    % Traditional nested call
    b(1, c, R1),
    a(R1, R2),
    d(R2, Result),
    writeln('Traditional nested calls result:'),
    writeln(Result).

test2 :-
    % What we want: b(1,c).a.d
    % This would be a new syntax that chains operations
    writeln('Method chain syntax would go here').

main :-
    test1,
    test2.

:- initialization(main, main).
