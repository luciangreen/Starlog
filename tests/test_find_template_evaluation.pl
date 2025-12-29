% test_find_template_evaluation.pl
% Tests for find/3 with template evaluation
% Verifies that templates containing Starlog expressions are evaluated

:- use_module('../starlog').

% Test 1: Exact problem statement - find(A:C, starlog_call([A:d:a:C] is [a:d:a:c]), Result)
test_problem_statement :-
    write('Test 1: find(A:C, starlog_call([A:d:a:C] is [a:d:a:c]), Result)...'),
    find(A:C, starlog_call([A:d:a:C] is [a:d:a:c]), Result),
    assertion(Result = "ac"),
    assertion(string(Result)),
    write(' ✓'), nl.

% Test 2: Similar pattern with different values
test_different_values :-
    write('Test 2: find(A:C, starlog_call([A:x:y:C] is [p:x:y:q]), Result)...'),
    find(A:C, starlog_call([A:x:y:C] is [p:x:y:q]), Result),
    assertion(Result = "pq"),
    assertion(string(Result)),
    write(' ✓'), nl.

% Test 3: Atom concatenation template
test_atom_concat_template :-
    write('Test 3: find(A•C, starlog_call([A•x•C] is [y•x•z]), Result)...'),
    find(A•C, starlog_call([A•x•C] is [y•x•z]), Result),
    assertion(Result = yz),
    assertion(atom(Result)),
    write(' ✓'), nl.

% Test 4: List append template
test_list_append_template :-
    write('Test 4: find(A&C, starlog_call(([1]&A&[2]) is ([1]&[3,4]&C)), Result)...'),
    find(A&C, starlog_call(([1]&A&[2]) is ([1]&[3,4]&C)), Result),
    assertion(Result = [3,4,2]),
    write(' ✓'), nl.

% Test 5: Template with three variables
test_three_variables :-
    write('Test 5: find(A:"-":C, starlog_call([A:"-":C] is ["x":"-":"y"]), Result)...'),
    find(A:"-":C, starlog_call([A:"-":C] is ["x":"-":"y"]), Result),
    assertion(Result = "x-y"),
    assertion(string(Result)),
    write(' ✓'), nl.

% Test 6: Non-Starlog template (should still work as before)
test_non_starlog_template :-
    write('Test 6: find(X, member(X, [1,2,3]), Result)...'),
    find(X, member(X, [1,2,3]), Result),
    assertion(Result = 1),
    write(' ✓'), nl.

% Test 7: List template (should still work as before)
test_list_template :-
    write('Test 7: find([A,C], starlog_call([A:a:C] is [a:a:c]), Result)...'),
    find([A,C], starlog_call([A:a:C] is [a:a:c]), Result),
    assertion(Result = [a, c]),
    assertion(is_list(Result)),
    write(' ✓'), nl.

% Test 8: Verify that plain templates still work
test_plain_template :-
    write('Test 8: find(result(A,C), starlog_call([A:x:C] is [p:x:q]), Result)...'),
    find(result(A,C), starlog_call([A:x:C] is [p:x:q]), Result),
    assertion(Result = result(p,q)),
    write(' ✓'), nl.

run_tests :-
    write('==================================================================='), nl,
    write('Testing find/3 with Template Evaluation'), nl,
    write('Verifies that Starlog expressions in templates are evaluated'), nl,
    write('==================================================================='), nl, nl,
    
    catch(test_problem_statement, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_different_values, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_atom_concat_template, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_list_append_template, E4, (write('✗ FAILED: '), write(E4), nl)),
    catch(test_three_variables, E5, (write('✗ FAILED: '), write(E5), nl)),
    catch(test_non_starlog_template, E6, (write('✗ FAILED: '), write(E6), nl)),
    catch(test_list_template, E7, (write('✗ FAILED: '), write(E7), nl)),
    catch(test_plain_template, E8, (write('✗ FAILED: '), write(E8), nl)),
    nl,
    
    write('==================================================================='), nl,
    write('All Template Evaluation Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
