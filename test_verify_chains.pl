:- use_module(starlog).

wrap_a(X, a(X)).
wrap_d(X, d(X)).

main :-
    writeln('Test 1: Single chain'),
    starlog_call(R1 is b(1,c) >> wrap_a),
    format('Result: ~w~n', [R1]),
    
    writeln('Test 2: Double chain'),
    starlog_call(R2 is b(1,c) >> wrap_a >> wrap_d),
    format('Result: ~w~n', [R2]),
    
    writeln('Test 3: Builtin chain'),
    starlog_call(R3 is reverse([1,2,3]) >> length),
    format('Result: ~w~n', [R3]),
    
    writeln('Test 4: Multiple builtins'),
    starlog_call(R4 is sort([3,1,2]) >> reverse),
    format('Result: ~w~n', [R4]),
    
    writeln('All tests completed successfully!').

:- initialization(main, main).
