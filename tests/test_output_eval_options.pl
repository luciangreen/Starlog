% test_output_eval_options.pl
% Tests for output_eval and output_no_eval options in starlog_output_code

:- use_module('../starlog_in_prolog').

% Test 1: Default behavior - strip both eval and no_eval
test_default_strip_both :-
    write('Test 1: Default - strip both eval() and no_eval()'), nl,
    write('  Input: A is no_eval(1+1)'), nl,
    write('  Output: '),
    starlog_output_code(A is no_eval(1+1)),
    write('  Input: B is eval("x":"y")'), nl,
    write('  Output: '),
    starlog_output_code(B is eval("x":"y")),
    nl.

% Test 2: Keep eval() only
test_keep_eval_only :-
    write('Test 2: Keep eval(), strip no_eval()'), nl,
    write('  Input: A is eval("x":"y")'), nl,
    write('  Output: '),
    starlog_output_code(A is eval("x":"y"), _, [output_eval(true)]),
    write('  Input: B is no_eval(1+1)'), nl,
    write('  Output: '),
    starlog_output_code(B is no_eval(1+1), _, [output_eval(true)]),
    nl.

% Test 3: Keep no_eval() only
test_keep_no_eval_only :-
    write('Test 3: Keep no_eval(), strip eval()'), nl,
    write('  Input: A is no_eval(1+1)'), nl,
    write('  Output: '),
    starlog_output_code(A is no_eval(1+1), _, [output_no_eval(true)]),
    write('  Input: B is eval("x":"y")'), nl,
    write('  Output: '),
    starlog_output_code(B is eval("x":"y"), _, [output_no_eval(true)]),
    nl.

% Test 4: Keep both eval() and no_eval()
test_keep_both :-
    write('Test 4: Keep both eval() and no_eval()'), nl,
    write('  Input: A is eval("x":"y")'), nl,
    write('  Output: '),
    starlog_output_code(A is eval("x":"y"), _, [output_eval(true), output_no_eval(true)]),
    write('  Input: B is no_eval(1+1)'), nl,
    write('  Output: '),
    starlog_output_code(B is no_eval(1+1), _, [output_eval(true), output_no_eval(true)]),
    nl.

% Test 5: Nested eval inside no_eval with different options
test_nested_eval_in_no_eval :-
    write('Test 5: Nested eval() inside no_eval()'), nl,
    write('  Input: A is no_eval("x" : eval("y":"z"))'), nl,
    write('  Default (strip both): '),
    starlog_output_code(A is no_eval("x" : eval("y":"z"))),
    write('  Keep both: '),
    starlog_output_code(B is no_eval("x" : eval("y":"z")), _, [output_eval(true), output_no_eval(true)]),
    write('  Keep no_eval only: '),
    starlog_output_code(C is no_eval("x" : eval("y":"z")), _, [output_no_eval(true)]),
    write('  Keep eval only: '),
    starlog_output_code(D is no_eval("x" : eval("y":"z")), _, [output_eval(true)]),
    nl.

% Test 6: Multiple expressions with eval/no_eval
test_multiple_expressions :-
    write('Test 6: Multiple expressions'), nl,
    write('  Input: (A is eval(1+1), B is no_eval(2*3))'), nl,
    write('  Default (strip both): '),
    starlog_output_code((A is eval(1+1), B is no_eval(2*3))),
    write('  Keep both: '),
    starlog_output_code((C is eval(1+1), D is no_eval(2*3)), _, [output_eval(true), output_no_eval(true)]),
    nl.

% Test 7: Complex nested structure
test_complex_nested :-
    write('Test 7: Complex nested structure'), nl,
    write('  Input: A is no_eval([eval(1+1), eval(2+2), 5])'), nl,
    write('  Default (strip both): '),
    starlog_output_code(A is no_eval([eval(1+1), eval(2+2), 5])),
    write('  Keep both: '),
    starlog_output_code(B is no_eval([eval(1+1), eval(2+2), 5]), _, [output_eval(true), output_no_eval(true)]),
    nl.

% Test 8: Verify return value matches stripped output
test_return_value :-
    write('Test 8: Verify return value matches stripped output'), nl,
    starlog_output_code(A is no_eval(1+1), Code1, []),
    write('  Default stripped code: '), write(Code1), nl,
    starlog_output_code(B is no_eval(1+1), Code2, [output_no_eval(true)]),
    write('  With no_eval preserved: '), write(Code2), nl,
    (Code1 = (_ is 1+1) -> write('  ✓ Default correctly strips no_eval()') ; write('  ✗ Default did not strip no_eval()')), nl,
    (Code2 = (_ is no_eval(1+1)) -> write('  ✓ Option correctly preserves no_eval()') ; write('  ✗ Option did not preserve no_eval()')), nl,
    nl.

% Run all tests
run_all_tests :-
    write('=== Running Output Eval/No_Eval Options Tests ==='), nl, nl,
    catch(test_default_strip_both, E1, (write('✗ Test 1 failed: '), write(E1), nl)),
    catch(test_keep_eval_only, E2, (write('✗ Test 2 failed: '), write(E2), nl)),
    catch(test_keep_no_eval_only, E3, (write('✗ Test 3 failed: '), write(E3), nl)),
    catch(test_keep_both, E4, (write('✗ Test 4 failed: '), write(E4), nl)),
    catch(test_nested_eval_in_no_eval, E5, (write('✗ Test 5 failed: '), write(E5), nl)),
    catch(test_multiple_expressions, E6, (write('✗ Test 6 failed: '), write(E6), nl)),
    catch(test_complex_nested, E7, (write('✗ Test 7 failed: '), write(E7), nl)),
    catch(test_return_value, E8, (write('✗ Test 8 failed: '), write(E8), nl)),
    write('=== All tests complete ==='), nl.

:- initialization(run_all_tests, main).
