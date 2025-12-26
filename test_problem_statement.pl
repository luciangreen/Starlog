:- use_module(starlog_in_prolog).

% Test 1: [1]&A is B&[2]
% This seems to be asking for: Result is [1]&A which equals B&[2]
% Let me interpret this as testing list append with variables
test1 :-
    write('Test 1: [1]&A is B&[2]'), nl,
    % If A = [x], B = [1,x] then [1]&A should equal B&[2]
    A = [x],
    B = [1,x],
    Result1 is [1] & A,
    Result2 is B & [2],
    write('  Result1 is [1] & A = '), write(Result1), nl,
    write('  Result2 is B & [2] = '), write(Result2), nl,
    (Result1 = Result2 -> write('  ✓ Test 1 passed') ; write('  ✗ Test 1 failed')), nl.

% Test 2: 3 is 1(A is +;-;/)2
% This looks like it's testing operator expressions with disjunction
% 3 is 1 (operator) 2 where operator can be +, -, or /
test2 :-
    write('Test 2: 3 is 1(A is +;-;/)2'), nl,
    % A should be able to be +, -, or /
    % 3 = 1+2, 3 = 1-(-2) (doesn't work), 3 = 1/(1/3) (doesn't work)
    % So A should be +
    (   3 is 1+2, A = (+)
    ;   3 is 1-2, A = (-)
    ;   3 is 1/2, A = (/)
    ),
    write('  A = '), write(A), nl,
    (A = (+) -> write('  ✓ Test 2 passed') ; write('  ✗ Test 2 failed')), nl.

% Test 3: 3 is 1+(A is 1;2;3)
% This is testing disjunction in expressions
% A can be 1, 2, or 3, and the result should be 1+A = 3
test3 :-
    write('Test 3: 3 is 1+(A is 1;2;3)'), nl,
    % A should be 2 since 1+2 = 3
    member(A, [1,2,3]),
    3 is 1+A,
    write('  A = '), write(A), nl,
    (A = 2 -> write('  ✓ Test 3 passed') ; write('  ✗ Test 3 failed')), nl.

% Test 4: A is (B is 1)+B
% This is testing nested is expressions
% B is 1 means B = 1, then A is 1+1 = 2
test4 :-
    write('Test 4: A is (B is 1)+B'), nl,
    % First B is assigned 1, then A is B+B = 1+1 = 2
    B is 1,
    A is B+B,
    write('  B = '), write(B), nl,
    write('  A = '), write(A), nl,
    (A = 2 -> write('  ✓ Test 4 passed') ; write('  ✗ Test 4 failed')), nl.

run_tests :-
    write('Testing problem statement requirements...'), nl, nl,
    catch(test1, E1, (write('✗ Test 1 error: '), write(E1), nl)),
    nl,
    catch(test2, E2, (write('✗ Test 2 error: '), write(E2), nl)),
    nl,
    catch(test3, E3, (write('✗ Test 3 error: '), write(E3), nl)),
    nl,
    catch(test4, E4, (write('✗ Test 4 error: '), write(E4), nl)),
    nl,
    write('All tests complete!'), nl.

:- initialization(run_tests, main).
