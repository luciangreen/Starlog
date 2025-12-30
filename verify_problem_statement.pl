% verify_problem_statement.pl
% Verification that the exact problem statement works correctly

:- use_module(starlog).

% The exact problem statement:
% starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result),C),C should be Result=[2,3]

test_problem_statement :-
    write('Testing: starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result),C)'), nl,
    write('Expected: After calling C, Result should be [2,3]'), nl, nl,
    
    % Call the exact expression from problem statement
    starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result), C),
    
    write('Generated code C: '), write(C), nl,
    write('Result before execution: '), write(Result), nl,
    
    % Execute the generated code
    starlog_call(C),
    
    write('Result after execution: '), write(Result), nl,
    
    % Verify it matches expected output
    (Result = [2,3] -> 
        write('✓ SUCCESS: Result = [2,3] as expected!'), nl
    ; 
        write('✗ FAILED: Result is not [2,3]'), nl,
        fail
    ).

:- initialization(test_problem_statement, main).
