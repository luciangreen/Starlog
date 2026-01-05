% demo_method_chain_simple.pl
% Simple demonstration of method chain syntax
% Shows how nested predicate calls d(a(b(1,C))) can be written as b(1,C) >> a >> d

:- use_module(starlog).

% Define helper predicates
wrap_a(X, a(X)).
wrap_b(X, Y, b(X, Y)).
wrap_d(X, d(X)).
add_one(X, Y) :- Y is X + 1.
double(X, Y) :- Y is X * 2.

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Method Chain Syntax Demonstration                    ║'),
    writeln('║  "d(a(b(1,C))) can be called as b(1,C) >> a >> d"     ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    % Basic method chains
    writeln('========================================'),
    writeln('Basic Method Chains'),
    writeln('========================================'),
    nl,
    
    writeln('Example 1: b(1,c) >> wrap_a'),
    starlog_call(R1 is b(1,c) >> wrap_a),
    format('Result: ~w~n', [R1]),
    nl,
    
    writeln('Example 2: b(1,c) >> wrap_a >> wrap_d'),
    starlog_call(R2 is b(1,c) >> wrap_a >> wrap_d),
    format('Result: ~w~n', [R2]),
    nl,
    
    % With built-in functions
    writeln('========================================'),
    writeln('Method Chains with Built-ins'),
    writeln('========================================'),
    nl,
    
    writeln('Example 3: reverse([1,2,3]) >> length'),
    starlog_call(R3 is reverse([1,2,3]) >> length),
    format('Result: ~w~n', [R3]),
    nl,
    
    writeln('Example 4: sort([3,1,2]) >> reverse'),
    starlog_call(R4 is sort([3,1,2]) >> reverse),
    format('Result: ~w~n', [R4]),
    nl,
    
    % With Starlog operators
    writeln('========================================'),
    writeln('Method Chains with Starlog Operators'),
    writeln('========================================'),
    nl,
    
    writeln('Example 5: ([1,2]&[3,4]) >> reverse'),
    starlog_call(R5 is ([1,2]&[3,4]) >> reverse),
    format('Result: ~w~n', [R5]),
    nl,
    
    writeln('Example 6: ("hello":"world") >> string_length'),
    starlog_call(R6 is ("hello":"world") >> string_length),
    format('Result: ~w~n', [R6]),
    nl,
    
    % Complex combinations
    writeln('========================================'),
    writeln('Complex Combinations'),
    writeln('========================================'),
    nl,
    
    writeln('Example 7: 5 >> add_one >> double'),
    starlog_call(R7 is 5 >> add_one >> double),
    format('Result: ~w~n', [R7]),
    nl,
    
    writeln('Example 8: ([1,2]&[3,4]) >> reverse >> length'),
    starlog_call(R8 is ([1,2]&[3,4]) >> reverse >> length),
    format('Result: ~w~n', [R8]),
    nl,
    
    % Problem statement verification
    writeln('========================================'),
    writeln('Problem Statement Verification'),
    writeln('========================================'),
    nl,
    
    writeln('Problem: d(a(b(1,C))) can be called as b(1,C) >> a >> d'),
    nl,
    
    writeln('Method chain: b(1,c) >> wrap_a >> wrap_d'),
    starlog_call(MC is b(1,c) >> wrap_a >> wrap_d),
    format('Result: ~w~n', [MC]),
    nl,
    
    writeln('Manual nested calls: wrap_d(wrap_a(wrap_b(1,c)))'),
    wrap_b(1, c, T1),
    wrap_a(T1, T2),
    wrap_d(T2, NC),
    format('Result: ~w~n', [NC]),
    nl,
    
    (MC = NC ->
        writeln('✓ SUCCESS: Method chain syntax matches nested calls!')
    ;
        writeln('✗ FAIL: Results do not match')
    ),
    nl,
    
    % Summary
    writeln('========================================'),
    writeln('Summary'),
    writeln('========================================'),
    writeln(''),
    writeln('The method chain syntax using >> operator provides:'),
    writeln('  ✓ Left-to-right reading (more natural flow)'),
    writeln('  ✓ Equivalent semantics to nested calls'),
    writeln('  ✓ Works with all Starlog expressions'),
    writeln('  ✓ Composable with existing features'),
    writeln(''),
    writeln('Syntax:  Base >> Method1 >> Method2 >> ...'),
    writeln('Example: reverse([1,2,3]) >> length  →  3'),
    writeln('Example: b(1,c) >> a >> d  →  d(a(b(1,c)))'),
    writeln(''),
    nl.

:- initialization(main, main).
