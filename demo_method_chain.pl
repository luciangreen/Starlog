% demo_method_chain.pl
% Comprehensive demonstration of method chain syntax
% Shows how nested predicate calls d(a(b(1,C))) can be written as b(1,C) >> a >> d

:- use_module(starlog).

demo_header(Title) :-
    nl,
    writeln('========================================'),
    writeln(Title),
    writeln('========================================').

demo_example(Description, GoalAtom, Expected) :-
    write('  '), write(Description), nl,
    write('    ?- '), write(GoalAtom), nl,
    % Parse the goal from atom
    atom_to_term(GoalAtom, Goal, []),
    (catch(call(Goal), E, (write('    ERROR: '), writeln(E), fail)) -> 
        (format('    Result: ~w~n', [Goal]),
         % Extract result from the goal
         (Goal = starlog_call(R is _) ->
            (R = Expected ->
                writeln('    ✓ PASS')
            ;
                format('    ✗ FAIL (Expected: ~w)~n', [Expected])
            )
         ;
            writeln('    ✓ PASS')
         ))
    ; 
        writeln('    ✗ FAIL: Goal failed')
    ),
    nl.

% ============================================================
% Section 1: Basic Method Chains
% ============================================================

% Define helper predicates
wrap_a(X, a(X)).
wrap_b(X, Y, b(X, Y)).
wrap_d(X, d(X)).

demo_basic :-
    demo_header('Section 1: Basic Method Chains'),
    
    demo_example(
        'Simple chain with binary predicate',
        'starlog_call(R is b(1,c) >> wrap_a)',
        a(b(1,c))
    ),
    
    demo_example(
        'Triple chain: b(1,c) >> wrap_a >> wrap_d',
        'starlog_call(R is b(1,c) >> wrap_a >> wrap_d)',
        d(a(b(1,c)))
    ),
    
    demo_example(
        'Chain with atom base',
        'starlog_call(R is hello >> wrap_a >> wrap_d)',
        d(a(hello))
    ).

% ============================================================
% Section 2: Method Chains with Built-in Functions
% ============================================================

demo_builtins :-
    demo_header('Section 2: Method Chains with Built-in Functions'),
    
    demo_example(
        'reverse >> length: Get length of reversed list',
        'starlog_call(R is reverse([1,2,3]) >> length)',
        3
    ),
    
    demo_example(
        'reverse >> reverse: Double reverse',
        'starlog_call(R is reverse([1,2,3]) >> reverse)',
        [1,2,3]
    ),
    
    demo_example(
        'sort >> reverse: Sort then reverse',
        'starlog_call(R is sort([3,1,2]) >> reverse)',
        [3,2,1]
    ),
    
    demo_example(
        'string_upper >> string_lower: Case transformations',
        'starlog_call(R is string_upper("hello") >> string_lower)',
        "hello"
    ).

% ============================================================
% Section 3: Method Chains with Starlog Operators
% ============================================================

demo_with_starlog_ops :-
    demo_header('Section 3: Method Chains with Starlog Operators'),
    
    demo_example(
        'String concat in base: ("a":"b") >> reverse',
        'starlog_call(R is ("a":"b") >> reverse)',
        "ba"
    ),
    
    demo_example(
        'List append in base: ([1,2]&[3,4]) >> reverse',
        'starlog_call(R is ([1,2]&[3,4]) >> reverse)',
        [4,3,2,1]
    ),
    
    demo_example(
        'Nested: ([1,2]&[3,4]) >> reverse >> length',
        'starlog_call(R is ([1,2]&[3,4]) >> reverse >> length)',
        4
    ),
    
    demo_example(
        'Method chain with atom concat: (a•b) >> wrap_a',
        'starlog_call(R is (a•b) >> wrap_a)',
        a(ab)
    ).

% ============================================================
% Section 4: Complex Combinations
% ============================================================

demo_complex :-
    demo_header('Section 4: Complex Combinations'),
    
    demo_example(
        'Multiple operations: reverse >> sort >> length',
        'starlog_call(R is reverse([3,1,2,3,1]) >> sort >> length)',
        3
    ),
    
    demo_example(
        'Starlog + chain: (reverse([1,2])&reverse([3,4])) >> flatten',
        'starlog_call(R is (reverse([1,2])&reverse([3,4])) >> flatten)',
        [2,1,4,3]
    ),
    
    demo_example(
        'Deep chain: [1,2,3] >> reverse >> reverse >> reverse',
        'starlog_call(R is [1,2,3] >> reverse >> reverse >> reverse)',
        [3,2,1]
    ).

% ============================================================
% Section 5: Equivalence with Nested Calls
% ============================================================

% Define predicates for demonstration
add_one(X, Y) :- Y is X + 1.
double(X, Y) :- Y is X * 2.

demo_equivalence :-
    demo_header('Section 5: Equivalence with Nested Calls'),
    
    writeln('  Demonstrating that method chains are equivalent to nested calls:'),
    nl,
    
    % Example 1: Simple chain
    starlog_call(R1 is 5 >> add_one >> double),
    % Equivalent nested calls
    add_one(5, T1),
    double(T1, R2),
    format('  Method chain:  5 >> add_one >> double = ~w~n', [R1]),
    format('  Nested calls:  double(add_one(5))     = ~w~n', [R2]),
    (R1 = R2 -> writeln('  ✓ Results match!') ; writeln('  ✗ Results differ')),
    nl,
    
    % Example 2: With list operations
    starlog_call(R3 is [1,2,3] >> reverse >> length),
    reverse([1,2,3], T3),
    length(T3, R4),
    format('  Method chain:  [1,2,3] >> reverse >> length = ~w~n', [R3]),
    format('  Nested calls:  length(reverse([1,2,3]))     = ~w~n', [R4]),
    (R3 = R4 -> writeln('  ✓ Results match!') ; writeln('  ✗ Results differ')),
    nl.

% ============================================================
% Section 6: Problem Statement Verification
% ============================================================

demo_problem_statement :-
    demo_header('Section 6: Problem Statement Verification'),
    
    writeln('  Problem: d(a(b(1,C))) can be called as b(1,C) >> a >> d'),
    nl,
    
    % Using method chain
    starlog_call(R1 is b(1,c) >> wrap_a >> wrap_d),
    
    % Using manual nested calls
    wrap_b(1, c, T1),
    wrap_a(T1, T2),
    wrap_d(T2, R2),
    
    format('  Method chain:      b(1,c) >> wrap_a >> wrap_d = ~w~n', [R1]),
    format('  Nested calls:      wrap_d(wrap_a(wrap_b(1,c))) = ~w~n', [R2]),
    nl,
    
    (R1 = R2 ->
        writeln('  ✓ SUCCESS: Method chain syntax matches nested calls!')
    ;
        writeln('  ✗ FAIL: Results do not match')
    ),
    nl,
    
    writeln('  The >> operator enables fluent, left-to-right reading:'),
    writeln('    - Start with base: b(1,c)'),
    writeln('    - Apply wrap_a: wrap_a(b(1,c))'),
    writeln('    - Apply wrap_d: wrap_d(wrap_a(b(1,c)))'),
    nl.

% ============================================================
% Section 7: All Combinations and Configurations
% ============================================================

demo_all_combinations :-
    demo_header('Section 7: All Combinations and Configurations'),
    
    writeln('  Method chains work with:'),
    writeln('    ✓ Value-returning builtins (reverse, length, etc.)'),
    writeln('    ✓ User-defined predicates'),
    writeln('    ✓ Starlog operators (:, &, •)'),
    writeln('    ✓ Nested Starlog expressions'),
    writeln('    ✓ Multiple levels of chaining'),
    writeln('    ✓ Mixed operations'),
    nl,
    
    demo_example(
        'All together: ([1,2]&[3]) >> reverse >> length',
        'starlog_call(R is ([1,2]&[3]) >> reverse >> length)',
        3
    ),
    
    demo_example(
        'String operations: ("hello":"world") >> string_length',
        'starlog_call(R is ("hello":"world") >> string_length)',
        10
    ),
    
    demo_example(
        'Atom operations: (a•b•c) >> atom_length',
        'starlog_call(R is (a•b•c) >> atom_length)',
        3
    ).

% ============================================================
% Main Demo
% ============================================================

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Method Chain Syntax Demonstration                    ║'),
    writeln('║  "d(a(b(1,C))) can be called as b(1,C) >> a >> d"     ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    demo_basic,
    demo_builtins,
    demo_with_starlog_ops,
    demo_complex,
    demo_equivalence,
    demo_problem_statement,
    demo_all_combinations,
    
    demo_header('Summary'),
    writeln('  The method chain syntax using >> operator provides:'),
    writeln('    ✓ Left-to-right reading (more natural flow)'),
    writeln('    ✓ Equivalent semantics to nested calls'),
    writeln('    ✓ Works with all Starlog expressions'),
    writeln('    ✓ Composable with existing features'),
    writeln('    ✓ Minimal implementation, fully backward compatible'),
    writeln(''),
    writeln('  Syntax:  Base >> Method1 >> Method2 >> ...'),
    writeln('  Example: reverse([1,2,3]) >> length  →  3'),
    writeln('  Example: b(1,c) >> a >> d  →  d(a(b(1,c)))'),
    writeln(''),
    nl.

:- initialization(main, main).
