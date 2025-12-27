% test_no_infinite_loop.pl
% Tests to ensure queries don't run forever after finding solutions

:- use_module('../starlog.pl').

% Test that list append queries terminate properly on backtracking
test_no_infinite_loop :-
    writeln('Testing that (A & [3]) is ["54",3] terminates on backtracking...'),
    % Find all solutions (should be exactly one)
    findall(A, starlog_call((A & [3]) is ["54",3]), Solutions),
    % Verify we got exactly one solution
    (Solutions = [["54"]] ->
        writeln('✓ Found exactly one solution: ["54"]'),
        writeln('✓ Query terminated properly (no infinite loop)')
    ;
        format('✗ FAILED: Expected [["54"]] but got ~w~n', [Solutions]),
        halt(1)
    ).

% Test with different list patterns
test_various_patterns :-
    writeln('Testing various list append patterns...'),
    
    % Pattern 1: Variable on left
    findall(X, starlog_call((X & [2]) is [1,2]), S1),
    (S1 = [[1]] ->
        writeln('✓ Pattern 1: (X & [2]) is [1,2] - passed')
    ;
        format('✗ Pattern 1 failed: ~w~n', [S1]),
        halt(1)
    ),
    
    % Pattern 2: Variable on right  
    findall(Y, starlog_call(([1] & Y) is [1,2]), S2),
    (S2 = [[2]] ->
        writeln('✓ Pattern 2: ([1] & Y) is [1,2] - passed')
    ;
        format('✗ Pattern 2 failed: ~w~n', [S2]),
        halt(1)
    ),
    
    % Pattern 3: Multiple elements
    findall(Z, starlog_call((Z & [3,4]) is [1,2,3,4]), S3),
    (S3 = [[1,2]] ->
        writeln('✓ Pattern 3: (Z & [3,4]) is [1,2,3,4] - passed')
    ;
        format('✗ Pattern 3 failed: ~w~n', [S3]),
        halt(1)
    ),
    
    % Pattern 4: Empty list cases
    findall(W, starlog_call((W & []) is [1,2,3]), S4),
    (S4 = [[1,2,3]] ->
        writeln('✓ Pattern 4: (W & []) is [1,2,3] - passed')
    ;
        format('✗ Pattern 4 failed: ~w~n', [S4]),
        halt(1)
    ).

% Run all tests
run_tests :-
    writeln('=== No Infinite Loop Tests ==='),
    writeln(''),
    test_no_infinite_loop,
    writeln(''),
    test_various_patterns,
    writeln(''),
    writeln('=== All No Infinite Loop Tests Passed ===').

% Run tests when file is loaded
:- initialization(run_tests, main).
