% tests/test_all_combinations_configurations.pl
% Comprehensive tests for: A is (1:1 >> string_number) * (+(1,1))
% Tests all combinations and configurations of:
% - String concatenation (1:1)
% - Method chaining (>> string_number)
% - Arithmetic operations (* (+(1,1)))

:- use_module('../starlog').

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
% Section 1: Core Pattern - Exact Problem Statement
% ============================================================

test_core_pattern :-
    writeln(''),
    writeln('Section 1: Core Pattern - (1:1 >> string_number) * (+(1,1))'),
    writeln('========================================'),
    
    % The exact pattern from problem statement
    test('A is (1:1 >> string_number) * (+(1,1))',
         starlog_call(A1 is (1:1 >> string_number) * (+(1,1))),
         22),
    
    % Verify intermediate steps
    test('A is 1:1 (string concat)',
         starlog_call(A2 is 1:1),
         "11"),
    
    test('A is string_number("11")',
         starlog_call(A3 is string_number("11")),
         11),
    
    test('A is 1:1 >> string_number',
         starlog_call(A4 is 1:1 >> string_number),
         11),
    
    test('A is +(1,1) (addition)',
         starlog_call(A5 is +(1,1)),
         2),
    
    test('A is 11 * 2 (arithmetic)',
         starlog_call(A6 is 11 * 2),
         22).

% ============================================================
% Section 2: Variations with Different Numbers
% ============================================================

test_number_variations :-
    writeln(''),
    writeln('Section 2: Variations with Different Numbers'),
    writeln('========================================'),
    
    % Different base numbers
    test('A is (2:3 >> string_number) * (+(1,1))',
         starlog_call(_A1 is (2:3 >> string_number) * (+(1,1))),
         46),  % "23" -> 23 * 2 = 46
    
    test('A is (4:5 >> string_number) * (+(2,2))',
         starlog_call(_A2 is (4:5 >> string_number) * (+(2,2))),
         180),  % "45" -> 45 * 4 = 180
    
    test('A is (1:0 >> string_number) * (+(3,3))',
         starlog_call(_A3 is (1:0 >> string_number) * (+(3,3))),
         60),  % "10" -> 10 * 6 = 60
    
    % Three-digit numbers
    test('A is (1:2:3 >> string_number) * (+(1,1))',
         starlog_call(_A4 is (1:2:3 >> string_number) * (+(1,1))),
         246).  % "123" -> 123 * 2 = 246

% ============================================================
% Section 3: Variations with Different Arithmetic Operators
% ============================================================

test_arithmetic_operators :-
    writeln(''),
    writeln('Section 3: Variations with Different Arithmetic Operators'),
    writeln('========================================'),
    
    % Addition
    test('A is (1:1 >> string_number) + (+(1,1))',
         starlog_call(_A5 is (1:1 >> string_number) + (+(1,1))),
         13),  % 11 + 2 = 13
    
    % Subtraction
    test('A is (1:1 >> string_number) - (+(1,1))',
         starlog_call(_A6 is (1:1 >> string_number) - (+(1,1))),
         9),  % 11 - 2 = 9
    
    % Division
    test('A is (2:2 >> string_number) / (+(1,1))',
         starlog_call(_A7 is (2:2 >> string_number) / (+(1,1))),
         11),  % 22 / 2 = 11
    
    % Integer division
    test('A is (2:3 >> string_number) // (+(2,2))',
         starlog_call(_A8 is (2:3 >> string_number) // (+(2,2))),
         5),  % 23 // 4 = 5
    
    % Modulo
    test('A is (2:3 >> string_number) mod (+(3,2))',
         starlog_call(_A9 is (2:3 >> string_number) mod (+(3,2))),
         3),  % 23 mod 5 = 3
    
    % Power (using **)
    test('A is (2:0 >> string_number) ** (+(1,1))',
         starlog_call(_A10 is (2:0 >> string_number) ** (+(1,1))),
         400),  % 20 ** 2 = 400
    
    % Power (using ^)
    test('A is (3:0 >> string_number) ^ (+(1,1))',
         starlog_call(_A11 is (3:0 >> string_number) ^ (+(1,1))),
         900).  % 30 ^ 2 = 900

% ============================================================
% Section 4: Variations with Different Method Chain Operations
% ============================================================

test_method_chain_variations :-
    writeln(''),
    writeln('Section 4: Variations with Different Method Chain Operations'),
    writeln('========================================'),
    
    % Using number_string instead
    test('A is (1:1 >> string_number >> number_string) : "x"',
         starlog_call(_A12 is (1:1 >> string_number >> number_string) : "x"),
         "11x"),
    
    % Multiple method chains
    test('A is ([1:1] >> reverse >> length)',
         starlog_call(_A13 is ([1:1] >> reverse >> length)),
         1),  % ["11"] reversed has length 1
    
    % String operations in chain
    test('A is (1:1 >> string_length)',
         starlog_call(_A14 is (1:1 >> string_length)),
         2).  % "11" has length 2

% ============================================================
% Section 5: Nested Combinations
% ============================================================

test_nested_combinations :-
    writeln(''),
    writeln('Section 5: Nested Combinations'),
    writeln('========================================'),
    
    % Nested string concatenation
    test('A is ((1:1) : (2:2) >> string_number) * 2',
         starlog_call(_A15 is ((1:1) : (2:2) >> string_number) * 2),
         2244),  % "1122" -> 1122 * 2 = 2244
    
    % Nested arithmetic
    test('A is (1:1 >> string_number) * (+(1,1) + +(1,1))',
         starlog_call(_A16 is (1:1 >> string_number) * (+(1,1) + +(1,1))),
         44),  % 11 * (2 + 2) = 44
    
    % Complex nested expression
    test('A is ((1:2 >> string_number) + (3:4 >> string_number)) * (+(1,1))',
         starlog_call(_A17 is ((1:2 >> string_number) + (3:4 >> string_number)) * (+(1,1))),
         92).  % (12 + 34) * 2 = 92

% ============================================================
% Section 6: Mixed Operator Combinations
% ============================================================

test_mixed_operators :-
    writeln(''),
    writeln('Section 6: Mixed Operator Combinations'),
    writeln('========================================'),
    
    % Multiple different operators in sequence
    test('A is ((1:1 >> string_number) * 2) + ((2:2 >> string_number) * 2)',
         starlog_call(_A18 is ((1:1 >> string_number) * 2) + ((2:2 >> string_number) * 2)),
         66),  % (11 * 2) + (22 * 2) = 22 + 44 = 66
    
    % Atom concatenation variant
    test('A is (a•b >> atom_length) * (+(1,1))',
         starlog_call(_A19 is (a•b >> atom_length) * (+(1,1))),
         4).  % ab has length 2, 2 * 2 = 4

% ============================================================
% Section 7: Edge Cases
% ============================================================

test_edge_cases :-
    writeln(''),
    writeln('Section 7: Edge Cases'),
    writeln('========================================'),
    
    % Zero in concatenation
    test('A is (0:0 >> string_number) * (+(1,1))',
         starlog_call(_A21 is (0:0 >> string_number) * (+(1,1))),
         0),  % "00" -> 0 * 2 = 0
    
    % Multiplication by zero
    test('A is (1:1 >> string_number) * (+(0,0))',
         starlog_call(_A22 is (1:1 >> string_number) * (+(0,0))),
         0),  % 11 * 0 = 0
    
    % Single digit
    test('A is (5 >> number_string) >> string_number',
         starlog_call(_A23 is (5 >> number_string) >> string_number),
         5),  % 5 -> "5" -> 5
    
    % Negative numbers (if supported)
    test('A is (1:1 >> string_number) * (+(0,-1))',
         starlog_call(_A24 is (1:1 >> string_number) * (+(0,-1))),
         -11).  % 11 * (-1) = -11

% ============================================================
% Section 8: All Configurations Summary
% ============================================================

test_all_configurations :-
    writeln(''),
    writeln('Section 8: All Configurations Summary'),
    writeln('========================================'),
    writeln(''),
    writeln('The following configurations are tested:'),
    writeln('  1. String concatenation: N:M'),
    writeln('  2. Method chaining: >> string_number'),
    writeln('  3. Arithmetic operations: *, +, -, /, //, mod, **, ^'),
    writeln('  4. Arithmetic function notation: +(N,M)'),
    writeln('  5. Nested combinations'),
    writeln('  6. Mixed operator types'),
    writeln('  7. Different number variations'),
    writeln('  8. Edge cases'),
    writeln('').

% ============================================================
% Main Test Runner
% ============================================================

run_all_tests :-
    init_tests,
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  All Combinations and Configurations Tests            ║'),
    writeln('║  Pattern: A is (1:1 >> string_number) * (+(1,1))      ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    test_core_pattern,
    test_number_variations,
    test_arithmetic_operators,
    test_method_chain_variations,
    test_nested_combinations,
    test_mixed_operators,
    test_edge_cases,
    test_all_configurations,
    
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
