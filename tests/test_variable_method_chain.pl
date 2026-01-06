% tests/test_variable_method_chain.pl
% Comprehensive tests for the pattern: B = Value, A is B>>operation>>operation
% This tests all combinations and configurations of variable-based method chaining

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

test(Name, Goal) :-
    retract(test_count(N)),
    N1 is N + 1,
    assertz(test_count(N1)),
    format('Test ~w: ~w~n', [N1, Name]),
    (catch(Goal, E, (format('  ERROR: ~w~n', [E]), fail)) ->
        format('  ✓ PASS~n'),
        retract(pass_count(P)),
        P1 is P + 1,
        assertz(pass_count(P1))
    ;
        format('  ✗ FAIL~n')
    ).

% ============================================================
% Section 1: Problem Statement Pattern
% ============================================================

test_problem_statement :-
    writeln(''),
    writeln('Section 1: Problem Statement - B = [3,2,2], A is B>>sort>>length'),
    writeln('----------------------------------------'),
    
    test('B = [3,2,2], A is B>>sort>>length, A = 2',
         (B = [3,2,2], starlog_call(A is B>>sort>>length), A = 2)),
    
    test('B = [3,2,2], A is B>>sort, A = [2,3]',
         (B = [3,2,2], starlog_call(A is B>>sort), A = [2,3])),
    
    test('B = [3,2,2], A is B>>length, A = 3',
         (B = [3,2,2], starlog_call(A is B>>length), A = 3)).

% ============================================================
% Section 2: List Variables with Different Method Chains
% ============================================================

test_list_variables :-
    writeln(''),
    writeln('Section 2: List Variables with Different Method Chains'),
    writeln('----------------------------------------'),
    
    test('B = [1,2,3], A is B>>reverse, A = [3,2,1]',
         (B = [1,2,3], starlog_call(A is B>>reverse), A = [3,2,1])),
    
    test('B = [5,1,3,2], A is B>>sort>>length, A = 4',
         (B = [5,1,3,2], starlog_call(A is B>>sort>>length), A = 4)),
    
    test('B = [[1,2],[3,4]], A is B>>flatten, A = [1,2,3,4]',
         (B = [[1,2],[3,4]], starlog_call(A is B>>flatten), A = [1,2,3,4])),
    
    test('B = [[1],[2],[3]], A is B>>flatten>>reverse, A = [3,2,1]',
         (B = [[1],[2],[3]], starlog_call(A is B>>flatten>>reverse), A = [3,2,1])),
    
    test('B = [5,2,8,1], A is B>>max_list, A = 8',
         (B = [5,2,8,1], starlog_call(A is B>>max_list), A = 8)),
    
    test('B = [5,2,8,1], A is B>>min_list, A = 1',
         (B = [5,2,8,1], starlog_call(A is B>>min_list), A = 1)),
    
    test('B = [1,2,3,4], A is B>>sum_list, A = 10',
         (B = [1,2,3,4], starlog_call(A is B>>sum_list), A = 10)).

% ============================================================
% Section 3: String Variables with Method Chains
% ============================================================

test_string_variables :-
    writeln(''),
    writeln('Section 3: String Variables with Method Chains'),
    writeln('----------------------------------------'),
    
    test('B = "hello", A is B>>string_length, A = 5',
         (B = "hello", starlog_call(A is B>>string_length), A = 5)),
    
    test('B = "hello", A is B>>string_upper, A = "HELLO"',
         (B = "hello", starlog_call(A is B>>string_upper), A = "HELLO")),
    
    test('B = "WORLD", A is B>>string_lower, A = "world"',
         (B = "WORLD", starlog_call(A is B>>string_lower), A = "world")),
    
    test('B = "hello", A is B>>string_upper>>string_length, A = 5',
         (B = "hello", starlog_call(A is B>>string_upper>>string_length), A = 5)),
    
    test('B = "test", A is B>>string_chars, A = [t,e,s,t]',
         (B = "test", starlog_call(A is B>>string_chars), A = [t,e,s,t])),
    
    test('B = "test", A is B>>string_chars>>length, A = 4',
         (B = "test", starlog_call(A is B>>string_chars>>length), A = 4)).

% ============================================================
% Section 4: Atom Variables with Method Chains
% ============================================================

test_atom_variables :-
    writeln(''),
    writeln('Section 4: Atom Variables with Method Chains'),
    writeln('----------------------------------------'),
    
    test('B = hello, A is B>>atom_length, A = 5',
         (B = hello, starlog_call(A is B>>atom_length), A = 5)),
    
    test('B = world, A is B>>atom_chars, A = [w,o,r,l,d]',
         (B = world, starlog_call(A is B>>atom_chars), A = [w,o,r,l,d])),
    
    test('B = abc, A is B>>atom_chars>>length, A = 3',
         (B = abc, starlog_call(A is B>>atom_chars>>length), A = 3)).

% ============================================================
% Section 5: Number Variables with Method Chains
% ============================================================

test_number_variables :-
    writeln(''),
    writeln('Section 5: Number Variables with Method Chains'),
    writeln('----------------------------------------'),
    
    test('B = 42, A is B>>abs, A = 42',
         (B = 42, starlog_call(A is B>>abs), A = 42)),
    
    test('B = -42, A is B>>abs, A = 42',
         (B = -42, starlog_call(A is B>>abs), A = 42)),
    
    test('B = 3.7, A is B>>ceiling, A = 4',
         (B = 3.7, starlog_call(A is B>>ceiling), A = 4)),
    
    test('B = 3.2, A is B>>floor, A = 3',
         (B = 3.2, starlog_call(A is B>>floor), A = 3)),
    
    test('B = 3.5, A is B>>round, A = 4',
         (B = 3.5, starlog_call(A is B>>round), A = 4)),
    
    test('B = 16, A is B>>sqrt, A = 4.0',
         (B = 16, starlog_call(A is B>>sqrt), A = 4.0)).

% ============================================================
% Section 6: Complex Chains (3+ operations)
% ============================================================

test_complex_chains :-
    writeln(''),
    writeln('Section 6: Complex Chains (3+ operations)'),
    writeln('----------------------------------------'),
    
    test('B = [3,1,2], A is B>>sort>>reverse>>length, A = 3',
         (B = [3,1,2], starlog_call(A is B>>sort>>reverse>>length), A = 3)),
    
    test('B = [[1,2],[3,4]], A is B>>flatten>>reverse>>length, A = 4',
         (B = [[1,2],[3,4]], starlog_call(A is B>>flatten>>reverse>>length), A = 4)),
    
    test('B = "hello", A is B>>string_upper>>string_chars>>length, A = 5',
         (B = "hello", starlog_call(A is B>>string_upper>>string_chars>>length), A = 5)).

% ============================================================
% Section 7: Variables from Starlog Operators
% ============================================================

test_operator_variables :-
    writeln(''),
    writeln('Section 7: Variables from Starlog Operators'),
    writeln('----------------------------------------'),
    
    test('B is [1,2]&[3,4], A is B>>reverse, A = [4,3,2,1]',
         (starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>reverse), A = [4,3,2,1])),
    
    test('B is [1,2]&[3,4], A is B>>sort>>length, A = 4',
         (starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>sort>>length), A = 4)),
    
    test('B is "hello":"world", A is B>>string_length, A = 10',
         (starlog_call(B is "hello":"world"), starlog_call(A is B>>string_length), A = 10)),
    
    test('B is "hello":"world", A is B>>string_upper, A = "HELLOWORLD"',
         (starlog_call(B is "hello":"world"), starlog_call(A is B>>string_upper), A = "HELLOWORLD")),
    
    test('B is reverse([1,2,3]), A is B>>length, A = 3',
         (starlog_call(B is reverse([1,2,3])), starlog_call(A is B>>length), A = 3)),
    
    test('B is sort([3,1,2]), A is B>>reverse, A = [3,2,1]',
         (starlog_call(B is sort([3,1,2])), starlog_call(A is B>>reverse), A = [3,2,1])).

% ============================================================
% Section 8: Multiple Variables in Sequence
% ============================================================

test_multiple_variables :-
    writeln(''),
    writeln('Section 8: Multiple Variables in Sequence'),
    writeln('----------------------------------------'),
    
    test('B = [3,2,2], C is B>>sort, A is C>>length, A = 2',
         (B = [3,2,2], starlog_call(C is B>>sort), starlog_call(A is C>>length), A = 2)),
    
    test('B = "hello", C is B>>string_upper, A is C>>string_length, A = 5',
         (B = "hello", starlog_call(C is B>>string_upper), starlog_call(A is C>>string_length), A = 5)),
    
    test('B = [1,2,3], C is B>>reverse, D is C>>sort, A is D>>length, A = 3',
         (B = [1,2,3], starlog_call(C is B>>reverse), starlog_call(D is C>>sort), starlog_call(A is D>>length), A = 3)).

% ============================================================
% Section 9: Edge Cases
% ============================================================

test_edge_cases :-
    writeln(''),
    writeln('Section 9: Edge Cases'),
    writeln('----------------------------------------'),
    
    test('B = [], A is B>>length, A = 0',
         (B = [], starlog_call(A is B>>length), A = 0)),
    
    test('B = [5], A is B>>sort>>length, A = 1',
         (B = [5], starlog_call(A is B>>sort>>length), A = 1)),
    
    test('B = "", A is B>>string_length, A = 0',
         (B = "", starlog_call(A is B>>string_length), A = 0)),
    
    test('B = [1,2,3], A is B>>sort, A = [1,2,3]',
         (B = [1,2,3], starlog_call(A is B>>sort), A = [1,2,3])),
    
    % Note: Prolog's sort/2 removes duplicates, so [1,1,1] becomes [1] with length 1
    test('B = [1,1,1], A is B>>sort>>length, A = 1',
         (B = [1,1,1], starlog_call(A is B>>sort>>length), A = 1)).

% ============================================================
% Main Test Runner
% ============================================================

run_all_tests :-
    init_tests,
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Variable Method Chain Tests                          ║'),
    writeln('║  Pattern: B = Value, A is B>>operation>>operation     ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    test_problem_statement,
    test_list_variables,
    test_string_variables,
    test_atom_variables,
    test_number_variables,
    test_complex_chains,
    test_operator_variables,
    test_multiple_variables,
    test_edge_cases,
    
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
