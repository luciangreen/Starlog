% test_method_chain_basic.pl
% Basic tests for method chain syntax using >> operator

:- use_module(starlog).

% Define test predicates that follow the value-returning pattern
% Each takes inputs and produces an output as the last argument

% a/2: wraps input in a(...)
a(X, a(X)).

% b/3: creates b(X, Y) from inputs X and Y
b(X, Y, b(X, Y)).

% d/2: wraps input in d(...)
d(X, d(X)).

% Test 1: Basic method chain - b(1,c) >> a should produce a(b(1,c))
test_basic_chain :-
    writeln('Test 1: Basic method chain b(1,c) >> a'),
    starlog_call(Result is b(1,c) >> a),
    writeln(Result),
    (Result = a(b(1,c)) ->
        writeln('✓ PASS: Basic method chain works')
    ;
        writeln('✗ FAIL: Expected a(b(1,c))')
    ),
    nl.

% Test 2: Triple method chain - b(1,c) >> a >> d should produce d(a(b(1,c)))
test_triple_chain :-
    writeln('Test 2: Triple method chain b(1,c) >> a >> d'),
    starlog_call(Result is b(1,c) >> a >> d),
    writeln(Result),
    (Result = d(a(b(1,c))) ->
        writeln('✓ PASS: Triple method chain works')
    ;
        writeln('✗ FAIL: Expected d(a(b(1,c)))')
    ),
    nl.

% Test 3: Method chain with registered builtins
test_builtin_chain :-
    writeln('Test 3: Method chain with builtins reverse([1,2,3]) >> length'),
    starlog_call(Result is reverse([1,2,3]) >> length),
    writeln(Result),
    (Result = 3 ->
        writeln('✓ PASS: Builtin method chain works')
    ;
        writeln('✗ FAIL: Expected 3')
    ),
    nl.

% Test 4: Compare with nested function syntax - ensure both produce same result
test_compare_nested :-
    writeln('Test 4: Compare method chain vs nested calls'),
    starlog_call(R1 is b(1,c) >> a >> d),
    % For nested calls, we need to manually call each
    b(1, c, T1),
    a(T1, T2),
    d(T2, R2),
    writeln('Method chain result: '), writeln(R1),
    writeln('Nested call result: '), writeln(R2),
    (R1 = R2 ->
        writeln('✓ PASS: Method chain equivalent to nested calls')
    ;
        writeln('✗ FAIL: Results should be identical')
    ),
    nl.

% Test 5: Method chain with list operations
test_list_chain :-
    writeln('Test 5: Method chain with list operations'),
    starlog_call(Result is reverse([1,2,3]) >> length),
    writeln('Length of reversed list:'), writeln(Result),
    (Result = 3 ->
        writeln('✓ PASS: Chained operations work')
    ;
        writeln('✗ FAIL')
    ),
    nl.

% Test 6: Simple atom chain
test_atom_chain :-
    writeln('Test 6: Simple method chain with atom'),
    starlog_call(Result is a >> d),
    writeln(Result),
    (Result = d(a) ->
        writeln('✓ PASS: Atom method chain works')
    ;
        writeln('✗ FAIL: Expected d(a)')
    ),
    nl.

% Test 7: Demonstrating equivalence to problem statement
% Problem: d(a(b(1,C))) can be called as b(1,C) >> a >> d
test_problem_statement :-
    writeln('Test 7: Problem statement - b(1,c) >> a >> d = manual d(a(b(1,c)))'),
    starlog_call(R1 is b(1,c) >> a >> d),
    % Manual nested calls
    b(1, c, T1),
    a(T1, T2),
    d(T2, R2),
    format('  b(1,c) >> a >> d           = ~w~n', [R1]),
    format('  Manual d(a(b(1,c)))        = ~w~n', [R2]),
    (R1 = R2 ->
        writeln('  ✓ PASS: Problem statement requirement met!')
    ;
        writeln('  ✗ FAIL')
    ),
    nl.

main :-
    writeln(''),
    writeln('========================================'),
    writeln('Method Chain Syntax Tests (>> operator)'),
    writeln('========================================'),
    writeln(''),
    writeln('Note: Using >> operator for method chaining'),
    writeln('Example: b(1,c) >> a >> d is equivalent to d(a(b(1,c)))'),
    writeln(''),
    test_basic_chain,
    test_triple_chain,
    test_builtin_chain,
    test_compare_nested,
    test_list_chain,
    test_atom_chain,
    test_problem_statement,
    writeln('========================================'),
    writeln('All tests completed'),
    writeln('========================================'),
    nl.

:- initialization(main, main).
