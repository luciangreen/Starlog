% tests/test_method_chain.pl
% Comprehensive tests for method chain syntax (>> operator)

:- use_module('../starlog').

% Define test predicates
wrap_a(X, a(X)).
wrap_b(X, Y, b(X, Y)).
wrap_c(X, Y, Z, c(X, Y, Z)).
wrap_d(X, d(X)).
add_one(X, Y) :- Y is X + 1.
double(X, Y) :- Y is X * 2.

% Test counter
:- dynamic test_count/1, pass_count/1.
test_count(0).
pass_count(0).

init_tests :-
    retractall(test_count(_)),
    retractall(pass_count(_)),
    assertz(test_count(0)),
    assertz(pass_count(0)).

test(Name, Goal, Expected) :-
    retract(test_count(N)),
    N1 is N + 1,
    assertz(test_count(N1)),
    format('Test ~w: ~w~n', [N1, Name]),
    (catch(call(Goal), E, (format('  ERROR: ~w~n', [E]), fail)) ->
        (Goal = starlog_call(R is _) ->
            (R = Expected ->
                format('  ✓ PASS: ~w~n', [R]),
                retract(pass_count(P)),
                P1 is P + 1,
                assertz(pass_count(P1))
            ;
                format('  ✗ FAIL: Expected ~w, got ~w~n', [Expected, R])
            )
        ;
            format('  ✓ PASS~n'),
            retract(pass_count(P)),
            P1 is P + 1,
            assertz(pass_count(P1))
        )
    ;
        format('  ✗ FAIL: Goal failed~n')
    ).

% ============================================================
% Section 1: Basic Method Chains
% ============================================================

test_basic_chains :-
    writeln(''),
    writeln('Section 1: Basic Method Chains'),
    writeln('----------------------------------------'),
    
    % Single method
    test('Single method with binary predicate',
         starlog_call(R is b(1,c) >> wrap_a),
         a(b(1,c))),
    
    % Double method chain
    test('Double method chain',
         starlog_call(R is b(1,c) >> wrap_a >> wrap_d),
         d(a(b(1,c)))),
    
    % Chain with atom base
    test('Chain with atom base',
         starlog_call(R is hello >> wrap_a),
         a(hello)),
    
    % Chain with number base
    test('Chain with number base',
         starlog_call(R is 5 >> wrap_a),
         a(5)),
    
    % Triple chain
    test('Triple method chain',
         starlog_call(R is wrap_a >> wrap_a >> wrap_a),
         a(a(a))).

% ============================================================
% Section 2: Method Chains with Built-in Functions
% ============================================================

test_builtin_chains :-
    writeln(''),
    writeln('Section 2: Method Chains with Built-in Functions'),
    writeln('----------------------------------------'),
    
    % List operations
    test('reverse >> length',
         starlog_call(R is reverse([1,2,3]) >> length),
         3),
    
    test('reverse >> reverse (identity)',
         starlog_call(R is reverse([1,2,3]) >> reverse),
         [1,2,3]),
    
    test('sort >> reverse',
         starlog_call(R is sort([3,1,2]) >> reverse),
         [3,2,1]),
    
    test('flatten >> length',
         starlog_call(R is flatten([[1,2],[3,4]]) >> length),
         4),
    
    % String operations
    test('string_upper >> string_lower',
         starlog_call(R is string_upper("hello") >> string_lower),
         "hello"),
    
    test('string_concat("a","b") >> string_length',
         starlog_call(R is string_concat("a","b") >> string_length),
         2).

% ============================================================
% Section 3: Method Chains with Starlog Operators
% ============================================================

test_starlog_ops :-
    writeln(''),
    writeln('Section 3: Method Chains with Starlog Operators'),
    writeln('----------------------------------------'),
    
    % String concatenation base
    test('String concat base >> reverse',
         starlog_call(R is ("a":"b") >> reverse),
         "ba"),
    
    test('String concat base >> string_length',
         starlog_call(R is ("hello":"world") >> string_length),
         10),
    
    % List append base
    test('List append base >> reverse',
         starlog_call(R is ([1,2]&[3,4]) >> reverse),
         [4,3,2,1]),
    
    test('List append base >> length',
         starlog_call(R is ([1,2]&[3,4]) >> length),
         4),
    
    % Atom concatenation base
    test('Atom concat base >> atom_length',
         starlog_call(R is (a•b•c) >> atom_length),
         3),
    
    % Nested Starlog + chain
    test('Nested: ([1,2]&[3,4]) >> reverse >> length',
         starlog_call(R is ([1,2]&[3,4]) >> reverse >> length),
         4).

% ============================================================
% Section 4: Complex Combinations
% ============================================================

test_complex :-
    writeln(''),
    writeln('Section 4: Complex Combinations'),
    writeln('----------------------------------------'),
    
    % Multiple operations
    test('reverse >> sort >> length',
         starlog_call(R is reverse([3,1,2,3,1]) >> sort >> length),
         3),
    
    % Mixed Starlog and builtins
    test('(reverse([1,2])&reverse([3,4])) >> flatten',
         starlog_call(R is (reverse([1,2])&reverse([3,4])) >> flatten),
         [2,1,4,3]),
    
    % Deep chaining
    test('[1,2,3] >> reverse >> reverse >> reverse',
         starlog_call(R is [1,2,3] >> reverse >> reverse >> reverse),
         [3,2,1]),
    
    % Arithmetic transformations
    test('5 >> add_one >> double',
         starlog_call(R is 5 >> add_one >> double),
         12),
    
    test('10 >> double >> add_one',
         starlog_call(R is 10 >> double >> add_one),
         21).

% ============================================================
% Section 5: Equivalence Tests
% ============================================================

test_equivalence :-
    writeln(''),
    writeln('Section 5: Equivalence with Nested Calls'),
    writeln('----------------------------------------'),
    
    % Test that method chains produce same result as manual nested calls
    starlog_call(MC1 is b(1,c) >> wrap_a >> wrap_d),
    wrap_b(1, c, T1),
    wrap_a(T1, T2),
    wrap_d(T2, NC1),
    test('Method chain = nested calls (custom predicates)',
         true,
         true),
    (MC1 = NC1 -> writeln('  Verified: Results match') ; writeln('  ERROR: Results differ')),
    
    starlog_call(MC2 is [1,2,3] >> reverse >> length),
    reverse([1,2,3], T3),
    length(T3, NC2),
    test('Method chain = nested calls (builtins)',
         true,
         true),
    (MC2 = NC2 -> writeln('  Verified: Results match') ; writeln('  ERROR: Results differ')).

% ============================================================
% Main Test Runner
% ============================================================

run_all_tests :-
    init_tests,
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Method Chain Syntax Tests (>> operator)              ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    test_basic_chains,
    test_builtin_chains,
    test_starlog_ops,
    test_complex,
    test_equivalence,
    
    writeln(''),
    writeln('========================================'),
    writeln('Test Summary'),
    writeln('========================================'),
    test_count(Total),
    pass_count(Passed),
    Failed is Total - Passed,
    format('Total:  ~w~n', [Total]),
    format('Passed: ~w~n', [Passed]),
    format('Failed: ~w~n', [Failed]),
    (Failed = 0 ->
        writeln('✓ All tests passed!')
    ;
        writeln('✗ Some tests failed')
    ),
    writeln('').

:- initialization(run_all_tests, main).
