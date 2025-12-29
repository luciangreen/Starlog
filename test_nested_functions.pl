% Test nested functions on LHS and RHS
:- use_module(starlog).

% Test 1: Nested on RHS (should work)
test1 :-
    write('Test 1: X is reverse(reverse([1,2,3]))... '),
    starlog_call(X is reverse(reverse([1,2,3]))),
    (X = [1,2,3] -> writeln('✓ PASS') ; writeln('✗ FAIL')).

% Test 2: Nested on LHS (should work but currently fails)
test2 :-
    write('Test 2: reverse(reverse([1,2,3])) is X... '),
    catch(
        (starlog_call(reverse(reverse([1,2,3])) is X), 
         (X = [1,2,3] -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 3: Functions on both sides (should work)
test3 :-
    write('Test 3: reverse(A) is reverse([1,2,3])... '),
    catch(
        (starlog_call(reverse(A) is reverse([1,2,3])),
         (A = [1,2,3] -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 4: Mixed - concat on LHS, function on RHS
test4 :-
    write('Test 4: (A:B) is reverse(["321"])... '),
    catch(
        (starlog_call((A:B) is reverse(["321"])),
         writeln('✓ PASS (A='), write(A), write(', B='), write(B), writeln(')')),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 5: Function on LHS, concat on RHS
test5 :-
    write('Test 5: reverse(A) is ("1":"2":"3")... '),
    catch(
        (starlog_call(reverse(A) is ("1":"2":"3")),
         (A = "321" -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 6: Nested function with concat on RHS
test6 :-
    write('Test 6: X is reverse("a":"b":"c")... '),
    catch(
        (starlog_call(X is reverse("a":"b":"c")),
         (X = "cba" -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 7: Nested function with concat on LHS
test7 :-
    write('Test 7: reverse("a":"b":"c") is X... '),
    catch(
        (starlog_call(reverse("a":"b":"c") is X),
         (X = "cba" -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 8: String concat with nested functions
test8 :-
    write('Test 8: (reverse("ab"):reverse("cd")) is X... '),
    catch(
        (starlog_call((reverse("ab"):reverse("cd")) is X),
         (X = "badc" -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

% Test 9: List append with nested functions
test9 :-
    write('Test 9: (reverse([1,2])&reverse([3,4])) is X... '),
    catch(
        (starlog_call((reverse([1,2])&reverse([3,4])) is X),
         (X = [2,1,4,3] -> writeln('✓ PASS') ; writeln('✗ FAIL'))),
        E,
        (writeln('✗ FAIL: '), writeln(E))
    ).

run_all_tests :-
    writeln('========================================'),
    writeln('Testing Nested Functions on LHS and RHS'),
    writeln('========================================'),
    nl,
    test1, nl,
    test2, nl,
    test3, nl,
    test4, nl,
    test5, nl,
    test6, nl,
    test7, nl,
    test8, nl,
    test9, nl,
    writeln('========================================'),
    writeln('Tests complete!'),
    writeln('========================================').

:- initialization(run_all_tests, main).
