% examples.pl
% Examples demonstrating Starlog-in-Prolog functionality
% Based on Requirements.txt acceptance tests

:- use_module(starlog).

% Example 1: Direct Starlog concat (Acceptance Test 1)
% Input: t(A) :- A is "x":"y".
% Behavior: t(A) yields "xy".
test_direct_concat :-
    t(Result),
    Result = "xy",
    write('✓ Example 1: Direct Starlog concat passed'), nl.

t(A) :- A is "x":"y".

% Example 2: List append (Acceptance Test 2)
% Input: t2(A) :- A is [1] & [2].
% Behavior: t2([1,2]).
test_list_append :-
    t2(Result),
    Result = [1,2],
    write('✓ Example 2: List append passed'), nl.

t2(A) :- A is [1] & [2].

% Example 3: Nested expression decompression (Acceptance Test 3)
% Input: t3(E) :- E is •("a":"b", "c").
% Behavior: E == 'abc'.
test_nested_expression :-
    t3(Result),
    atom_string(Result, S),
    S = "abc",
    write('✓ Example 3: Nested expression decompression passed'), nl.

t3(E) :- E is •("a":"b", "c").

% Example 4: Arithmetic is preserved (Acceptance Test 4)
% Input: t4(X) :- X is 1+2.
% Behavior: X = 3 and no Starlog rewriting occurs.
test_arithmetic_preserved :-
    t4(Result),
    Result = 3,
    write('✓ Example 4: Arithmetic is preserved passed'), nl.

t4(X) :- X is 1+2.

% Example 5: Complex nested expression from Requirements.txt
% E is •(A:(B:(D•F)), C)
test_complex_nested :-
    complex_concat("A", "B", "D", "F", "C", E),
    atom_string(E, S),
    S = "ABDFC",
    write('✓ Example 5: Complex nested expression passed'), nl.

complex_concat(A, B, D, F, C, E) :-
    E is •(A:(B:(D•F)), C).

% Example 6: Value-returning builtin
test_value_builtin :-
    test_reverse([1,2,3], R),
    R = [3,2,1],
    write('✓ Example 6: Value-returning builtin passed'), nl.

test_reverse(L, R) :- R is reverse(L).

% Example 7: Coexistence with normal Prolog
test_coexistence :-
    normal_prolog([1,2,3], L),
    L = 3,
    write('✓ Example 7: Coexistence with normal Prolog passed'), nl.

normal_prolog(List, Len) :- 
    length(List, Len).

% Run all examples
run_all_examples :-
    write('Running Starlog-in-Prolog examples...'), nl, nl,
    catch(test_direct_concat, E1, (write('✗ Example 1 failed: '), write(E1), nl)),
    catch(test_list_append, E2, (write('✗ Example 2 failed: '), write(E2), nl)),
    catch(test_nested_expression, E3, (write('✗ Example 3 failed: '), write(E3), nl)),
    catch(test_arithmetic_preserved, E4, (write('✗ Example 4 failed: '), write(E4), nl)),
    catch(test_complex_nested, E5, (write('✗ Example 5 failed: '), write(E5), nl)),
    catch(test_value_builtin, E6, (write('✗ Example 6 failed: '), write(E6), nl)),
    catch(test_coexistence, E7, (write('✗ Example 7 failed: '), write(E7), nl)),
    nl,
    write('All examples complete!'), nl.

:- initialization(run_all_examples, main).
