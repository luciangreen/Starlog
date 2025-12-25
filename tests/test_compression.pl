% test_compression.pl
% Tests for Starlog maximal compression feature

:- use_module('../starlog_in_prolog').

% Helper to capture output
capture_output(Goal, Output) :-
    with_output_to(string(Output), Goal).

% Test 1: Simple nested string concatenation
test_simple_nesting :-
    write('Test 1: Simple nested string concatenation'), nl,
    capture_output(
        starlog_output_code((string_concat('hello',' ',T1), string_concat(T1,'world',T2)), _, [compress(true)]),
        Output
    ),
    (sub_string(Output, _, _, _, "hello:' ':world") ->
        write('✓ Simple nesting test passed')
    ;
        write('✗ Simple nesting test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 2: Nested list operations
test_list_nesting :-
    write('Test 2: Nested list operations'), nl,
    capture_output(
        starlog_output_code((append([1],[2],L1), reverse(L1,L2)), _, [compress(true)]),
        Output
    ),
    (sub_string(Output, _, _, _, "reverse([1]&[2])") ->
        write('✓ List nesting test passed')
    ;
        write('✗ List nesting test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 3: Variable used multiple times (should NOT nest)
test_multiple_uses :-
    write('Test 3: Variable used multiple times'), nl,
    capture_output(
        starlog_output_code((string_concat('hello',' ',T1), string_concat(T1,'world',T2), string_concat(T1,'friend',T3)), _, [compress(true)]),
        Output
    ),
    % T1 should remain as a separate variable since it's used twice
    (sub_string(Output, _, _, _, "hello:' '") ->
        write('✓ Multiple uses test passed')
    ;
        write('✗ Multiple uses test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 4: Multiple nested expressions
test_multiple_nesting :-
    write('Test 4: Multiple nested expressions'), nl,
    capture_output(
        starlog_output_code((string_concat('a','b',T1), string_concat('c','d',T2), string_concat(T1,T2,T3)), _, [compress(true)]),
        Output
    ),
    (sub_string(Output, _, _, _, "a:b:(c:d)") ->
        write('✓ Multiple nesting test passed')
    ;
        write('✗ Multiple nesting test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 5: Compression disabled (default)
test_no_compression :-
    write('Test 5: No compression (default)'), nl,
    capture_output(
        starlog_output_code((string_concat('hello',' ',T1), string_concat(T1,'world',T2)), _),
        Output
    ),
    % Should have two separate assignments
    (sub_string(Output, _, _, _, "A is hello:' ',B is A:world") ->
        write('✓ No compression test passed')
    ;
        write('✗ No compression test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 6: Long chain of nesting
test_long_chain :-
    write('Test 6: Long chain of nesting'), nl,
    capture_output(
        starlog_output_code((
            string_concat('a','b',T1),
            string_concat(T1,'c',T2),
            string_concat(T2,'d',T3)
        ), _, [compress(true)]),
        Output
    ),
    (sub_string(Output, _, _, _, "a:b:c:d") ->
        write('✓ Long chain test passed')
    ;
        write('✗ Long chain test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 7: Mixed operations
test_mixed_operations :-
    write('Test 7: Mixed operations'), nl,
    capture_output(
        starlog_output_code((
            append([1,2],[3],L1),
            reverse(L1,L2),
            string_length("hello",Len)
        ), _, [compress(true)]),
        Output
    ),
    % L1 should be nested, but Len should be separate
    (sub_string(Output, _, _, _, "reverse([1,2]&[3])") ->
        write('✓ Mixed operations test passed')
    ;
        write('✗ Mixed operations test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Test 8: Atom concatenation nesting
test_atom_concat_nesting :-
    write('Test 8: Atom concatenation nesting'), nl,
    capture_output(
        starlog_output_code((atom_concat(hello,world,A1), atom_concat(A1,test,A2)), _, [compress(true)]),
        Output
    ),
    (sub_string(Output, _, _, _, "hello•world•test") ->
        write('✓ Atom concat nesting test passed')
    ;
        write('✗ Atom concat nesting test failed'), nl,
        write('Output: '), write(Output)
    ),
    nl.

% Run all tests
run_tests :-
    write('=== Running Starlog Compression Tests ==='), nl, nl,
    catch(test_simple_nesting, E1, (write('✗ Test 1 exception: '), write(E1), nl)),
    catch(test_list_nesting, E2, (write('✗ Test 2 exception: '), write(E2), nl)),
    catch(test_multiple_uses, E3, (write('✗ Test 3 exception: '), write(E3), nl)),
    catch(test_multiple_nesting, E4, (write('✗ Test 4 exception: '), write(E4), nl)),
    catch(test_no_compression, E5, (write('✗ Test 5 exception: '), write(E5), nl)),
    catch(test_long_chain, E6, (write('✗ Test 6 exception: '), write(E6), nl)),
    catch(test_mixed_operations, E7, (write('✗ Test 7 exception: '), write(E7), nl)),
    catch(test_atom_concat_nesting, E8, (write('✗ Test 8 exception: '), write(E8), nl)),
    nl,
    write('=== Compression tests complete ==='), nl.

:- initialization(run_tests, main).
