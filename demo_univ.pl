% demo_univ.pl
% Demo script showcasing the univ operator support in Starlog

:- use_module(starlog).

demo :-
    write('=============================================='), nl,
    write('Starlog Univ Operator Demo'), nl,
    write('=============================================='), nl, nl,

    write('Example 1: List to Term Conversion'), nl,
    write('  Code: T is ..=([f,0,1])'), nl,
    starlog_call((T is ..=([f,0,1]))),
    write('  Result: T = '), write(T), nl, nl,

    write('Example 2: Term to List Conversion'), nl,
    write('  Code: L is =..(f(0,1))'), nl,
    starlog_call((L is =..(f(0,1)))),
    write('  Result: L = '), write(L), nl, nl,

    write('Example 3: Roundtrip Conversion'), nl,
    write('  Code: T1 is ..=([bar,x,y]), L1 is =..(T1)'), nl,
    starlog_call((T1 is ..=([bar,x,y]), L1 is =..(T1))),
    write('  Result: T1 = '), write(T1), nl,
    write('          L1 = '), write(L1), nl, nl,

    write('Example 4: Building Complex Terms'), nl,
    write('  Code: Complex is ..=([factorial,10])'), nl,
    starlog_call((Complex is ..=([factorial,10]))),
    write('  Result: Complex = '), write(Complex), nl, nl,

    write('Example 5: Deconstructing Nested Terms'), nl,
    write('  Code: Parts is =..(foo(bar(1,2),baz(3,4)))'), nl,
    starlog_call((Parts is =..(foo(bar(1,2),baz(3,4))))),
    write('  Result: Parts = '), write(Parts), nl, nl,

    write('=============================================='), nl,
    write('Demo complete!'), nl,
    write('=============================================='), nl.

:- initialization(demo, main).
