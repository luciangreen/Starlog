% test_foldr.pl
% Tests for foldr implementation

:- use_module(starlog).

% Test 1: Basic foldr with string_concat in pure Prolog
test_foldr_basic_prolog :-
    write('Test 1: Basic foldr with string_concat (Prolog)'), nl,
    foldr(string_concat, [c, b, a], "", Result),
    write('  foldr(string_concat, [c, b, a], "", Result)'), nl,
    write('  Result: '), write(Result), nl,
    Result = "cba",
    write('✓ Test 1 passed'), nl, nl.

% Test 2: The exact problem statement case in Prolog
test_foldr_problem_statement_prolog :-
    write('Test 2: Problem statement case (Prolog)'), nl,
    string_chars("abc", Chars),
    write('  string_chars("abc") = '), write(Chars), nl,
    reverse(Chars, RevChars),
    write('  reverse(Chars) = '), write(RevChars), nl,
    foldr(string_concat, RevChars, "", Result),
    write('  foldr(string_concat, RevChars, "", Result) = '), write(Result), nl,
    Result = "cba",
    write('✓ Test 2 passed'), nl, nl.

% Test 3: Nested call version - the problem asks for this
test_foldr_nested_prolog :-
    write('Test 3: Nested foldr call (Prolog)'), nl,
    string_chars("abc", Chars1),
    reverse(Chars1, RevChars1),
    foldr(string_concat, RevChars1, "", Result),
    write('  foldr(string_concat, reverse(string_chars("abc")), "", Result) = '), write(Result), nl,
    Result = "cba",
    write('✓ Test 3 passed'), nl, nl.

% Test 4: Using Starlog syntax with starlog_call
test_foldr_starlog :-
    write('Test 4: foldr with Starlog syntax'), nl,
    starlog_call(Result is foldr(string_concat, reverse(string_chars("abc")), "")),
    write('  Result is foldr(string_concat, reverse(string_chars("abc")), "") = '), write(Result), nl,
    Result = "cba",
    write('✓ Test 4 passed'), nl, nl.

% Test 5: Different test - foldr with numbers becoming strings
test_foldr_different :-
    write('Test 5: foldr with different input'), nl,
    starlog_call(Result is foldr(string_concat, reverse(string_chars("xyz")), "")),
    write('  Result is foldr(string_concat, reverse(string_chars("xyz")), "") = '), write(Result), nl,
    Result = "zyx",
    write('✓ Test 5 passed'), nl, nl.

% Test 6: foldr with list append
test_foldr_append :-
    write('Test 6: foldr with append'), nl,
    foldr(append, [[1], [2], [3]], [], Result),
    write('  foldr(append, [[1], [2], [3]], [], Result) = '), write(Result), nl,
    Result = [1, 2, 3],
    write('✓ Test 6 passed'), nl, nl.

% Test 7: Empty list
test_foldr_empty :-
    write('Test 7: foldr with empty list'), nl,
    foldr(string_concat, [], "init", Result),
    write('  foldr(string_concat, [], "init", Result) = '), write(Result), nl,
    Result = "init",
    write('✓ Test 7 passed'), nl, nl.

% Run all tests
run_tests :-
    write('=== Running foldr tests ==='), nl, nl,
    catch(test_foldr_basic_prolog, E1, (write('✗ Test 1 failed: '), write(E1), nl, nl)),
    catch(test_foldr_problem_statement_prolog, E2, (write('✗ Test 2 failed: '), write(E2), nl, nl)),
    catch(test_foldr_nested_prolog, E3, (write('✗ Test 3 failed: '), write(E3), nl, nl)),
    catch(test_foldr_starlog, E4, (write('✗ Test 4 failed: '), write(E4), nl, nl)),
    catch(test_foldr_different, E5, (write('✗ Test 5 failed: '), write(E5), nl, nl)),
    catch(test_foldr_append, E6, (write('✗ Test 6 failed: '), write(E6), nl, nl)),
    catch(test_foldr_empty, E7, (write('✗ Test 7 failed: '), write(E7), nl, nl)),
    write('=== All foldr tests complete ==='), nl.

:- initialization(run_tests, main).
