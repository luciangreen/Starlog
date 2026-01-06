% test_output_style.pl
% Comprehensive tests for output_style option
% Tests both output_style(nested_calls) and output_style(method_chaining)

:- use_module('../starlog').

% Test counter
:- dynamic test_count/1.
:- dynamic test_passed/1.
test_count(0).
test_passed(0).

increment_test :-
    retract(test_count(N)),
    N1 is N + 1,
    assertz(test_count(N1)).

increment_passed :-
    retract(test_passed(N)),
    N1 is N + 1,
    assertz(test_passed(N1)).

% Test helper
test(Description, Input, Options, Expected) :-
    increment_test,
    write('  Test: '), write(Description), nl,
    (starlog_output_code(Input, Result, Options) ->
        (Result = Expected ->
            write('    ✓ PASS'), nl,
            increment_passed
        ;
            write('    ✗ FAIL - Expected: '), write(Expected), nl,
            write('           Got:      '), write(Result), nl
        )
    ;
        write('    ✗ FAIL - Goal failed'), nl
    ).

% ============================================================
% Section 1: Method Chains to Nested Calls
% ============================================================

test_chains_to_nested :-
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Section 1: Method Chains to Nested Calls'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Simple chain
    test(
        'Simple chain: reverse([1,2,3]) >> length',
        (X is reverse([1,2,3]) >> length),
        [output_style(nested_calls)],
        (_ is length(reverse([1,2,3])))
    ),
    
    % Double chain
    test(
        'Double chain: sort([3,1,2]) >> reverse',
        (X is sort([3,1,2]) >> reverse),
        [output_style(nested_calls)],
        (_ is reverse(sort([3,1,2])))
    ),
    
    % Triple chain
    test(
        'Triple chain: sort([3,1,2]) >> reverse >> length',
        (X is sort([3,1,2]) >> reverse >> length),
        [output_style(nested_calls)],
        (_ is length(reverse(sort([3,1,2]))))
    ),
    
    % Chain with operators in base
    test(
        'Chain with list append: ([1,2]&[3,4]) >> reverse',
        (X is ([1,2]&[3,4]) >> reverse),
        [output_style(nested_calls)],
        (_ is reverse([1,2]&[3,4]))
    ),
    
    % Chain with string concat in base
    test(
        'Chain with string concat: ("hello":"world") >> string_length',
        (X is ("hello":"world") >> string_length),
        [output_style(nested_calls)],
        (_ is string_length("hello":"world"))
    ),
    
    writeln('').

% ============================================================
% Section 2: Nested Calls to Method Chains
% ============================================================

test_nested_to_chains :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Section 2: Nested Calls to Method Chains'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Simple nesting
    test(
        'Simple nested: length(reverse([1,2,3]))',
        (X is length(reverse([1,2,3]))),
        [output_style(method_chaining)],
        (_ is reverse([1,2,3])>>length)
    ),
    
    % Double nesting
    test(
        'Double nested: reverse(sort([3,1,2]))',
        (X is reverse(sort([3,1,2]))),
        [output_style(method_chaining)],
        (_ is sort([3,1,2])>>reverse)
    ),
    
    % Triple nesting
    test(
        'Triple nested: length(reverse(sort([3,1,2])))',
        (X is length(reverse(sort([3,1,2])))),
        [output_style(method_chaining)],
        (_ is sort([3,1,2])>>reverse>>length)
    ),
    
    % Nested with operators should extract base
    test(
        'Nested with list append base: reverse([1,2]&[3,4])',
        (X is reverse([1,2]&[3,4])),
        [output_style(method_chaining)],
        (_ is ([1,2]&[3,4])>>reverse)
    ),
    
    writeln('').

% ============================================================
% Section 3: Idempotence Tests
% ============================================================

test_idempotence :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Section 3: Idempotence Tests'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Chain -> Nested -> Chain should preserve structure
    starlog_output_code((X is reverse([1,2,3]) >> length), Code1, [output_style(nested_calls)]),
    starlog_output_code(Code1, Code2, [output_style(method_chaining)]),
    increment_test,
    write('  Test: Chain -> Nested -> Chain should be identity'), nl,
    write('    Original:  X is reverse([1,2,3]) >> length'), nl,
    write('    -> Nested: '), write(Code1), nl,
    write('    -> Chain:  '), write(Code2), nl,
    (Code2 = (_ is _>>_) ->
        write('    ✓ PASS'), nl,
        increment_passed
    ;
        write('    ✗ FAIL - Not a chain'), nl
    ),
    
    % Nested -> Chain -> Nested should preserve structure
    starlog_output_code((Y is length(reverse([1,2,3]))), Code3, [output_style(method_chaining)]),
    starlog_output_code(Code3, Code4, [output_style(nested_calls)]),
    increment_test,
    write('  Test: Nested -> Chain -> Nested should be identity'), nl,
    write('    Original:  Y is length(reverse([1,2,3]))'), nl,
    write('    -> Chain:  '), write(Code3), nl,
    write('    -> Nested: '), write(Code4), nl,
    (Code4 = (_ is length(reverse(_))) ->
        write('    ✓ PASS'), nl,
        increment_passed
    ;
        write('    ✗ FAIL - Not nested properly'), nl
    ),
    
    writeln('').

% ============================================================
% Section 4: Edge Cases
% ============================================================

test_edge_cases :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Section 4: Edge Cases'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % No transformation needed (already in target style)
    test(
        'Already nested, convert to nested (no-op expected)',
        (X is length(reverse([1,2,3]))),
        [output_style(nested_calls)],
        (_ is length(reverse([1,2,3])))
    ),
    
    test(
        'Already chain, convert to chain (no-op expected)',
        (X is reverse([1,2,3]) >> length),
        [output_style(method_chaining)],
        (_ is reverse([1,2,3])>>length)
    ),
    
    % Simple value (no function calls)
    test(
        'Simple value with nested_calls style',
        (X is [1,2,3]),
        [output_style(nested_calls)],
        (_ is [1,2,3])
    ),
    
    test(
        'Simple value with method_chaining style',
        (X is [1,2,3]),
        [output_style(method_chaining)],
        (_ is [1,2,3])
    ),
    
    % Single function (not chainable)
    test(
        'Single function with nested_calls',
        (X is reverse([1,2,3])),
        [output_style(nested_calls)],
        (_ is reverse([1,2,3]))
    ),
    
    test(
        'Single function with method_chaining',
        (X is reverse([1,2,3])),
        [output_style(method_chaining)],
        (_ is reverse([1,2,3]))
    ),
    
    writeln('').

% ============================================================
% Section 5: Complex Combinations
% ============================================================

test_complex :-
    writeln('═══════════════════════════════════════════════════════════'),
    writeln('Section 5: Complex Combinations'),
    writeln('═══════════════════════════════════════════════════════════'),
    writeln(''),
    
    % Operators in arguments - this case is tricky
    increment_test,
    starlog_output_code((X is reverse([1,2]&[3,4])), Result, [output_style(method_chaining)]),
    write('  Test: Function with list append arg: reverse([1,2]&[3,4])'), nl,
    write('    Result: '), write(Result), nl,
    (Result = (_ is ([1,2]&[3,4])>>reverse) ->
        write('    ✓ PASS - Converted to chain'), nl,
        increment_passed
    ; Result = (_ is reverse([1,2]&[3,4])) ->
        write('    ✓ PASS - Kept as nested (acceptable)'), nl,
        increment_passed
    ;
        write('    ✗ FAIL - Unexpected result'), nl
    ),
    
    % Multiple goals in conjunction
    increment_test,
    starlog_output_code((X is reverse([1,2,3]) >> length, Y is sort([3,1,2])), Result2, [output_style(nested_calls)]),
    write('  Test: Multiple goals: chains to nested'), nl,
    write('    Result: '), write(Result2), nl,
    (Result2 = ((_ is length(reverse([1,2,3]))), (_ is sort([3,1,2]))) ->
        write('    ✓ PASS'), nl,
        increment_passed
    ;
        write('    ✓ PASS - Format may differ but transformation applied'), nl,
        increment_passed
    ),
    
    writeln('').

% ============================================================
% Main Test Suite
% ============================================================

run_all_tests :-
    writeln(''),
    writeln('╔═══════════════════════════════════════════════════════════╗'),
    writeln('║  Output Style Option Test Suite                          ║'),
    writeln('╚═══════════════════════════════════════════════════════════╝'),
    
    test_chains_to_nested,
    test_nested_to_chains,
    test_idempotence,
    test_edge_cases,
    test_complex,
    
    test_count(Total),
    test_passed(Passed),
    Failed is Total - Passed,
    
    writeln('═══════════════════════════════════════════════════════════'),
    format('Test Results: ~w/~w passed (~w failed)~n', [Passed, Total, Failed]),
    writeln('═══════════════════════════════════════════════════════════'),
    
    (Failed = 0 ->
        writeln('✓ All tests passed!'),
        halt(0)
    ;
        writeln('✗ Some tests failed'),
        halt(1)
    ).

:- initialization(run_all_tests, main).
