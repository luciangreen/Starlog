% comprehensive_univ_test.pl
% Comprehensive test demonstrating all aspects of univ operator support

:- use_module('../starlog').

% Test 1: Basic list to term
test1 :-
    write('Test 1: Basic list to term conversion'), nl,
    starlog_call((Result is ..=([append,[1,2],[3,4]]))),
    Result = append([1,2],[3,4]),
    write('  ✓ PASS: Created term append([1,2],[3,4])'), nl, nl.

% Test 2: Basic term to list  
test2 :-
    write('Test 2: Basic term to list conversion'), nl,
    starlog_call((Result is =..(member(5,[1,2,3,4,5])))),
    Result = [member,5,[1,2,3,4,5]],
    write('  ✓ PASS: Converted term to list [member,5,[1,2,3,4,5]]'), nl, nl.

% Test 3: Combined with other Starlog operators
test3 :-
    write('Test 3: Combining univ with string concatenation'), nl,
    starlog_call((Str is "foo":"bar", Term is ..=([myfunc,1,2]), Parts is =..(Term))),
    Str = "foobar",
    Term = myfunc(1,2),
    Parts = [myfunc,1,2],
    write('  ✓ PASS: Combined operators work correctly'), nl, nl.

% Test 4: Nested expressions
test4 :-
    write('Test 4: Nested term construction'), nl,
    starlog_call((Inner is ..=([foo,a]), List is =..(Inner))),
    Inner = foo(a),
    List = [foo,a],
    write('  ✓ PASS: Nested expressions handled correctly'), nl, nl.

% Test 5: With list append
test5 :-
    write('Test 5: Combining univ with list append'), nl,
    starlog_call((Parts1 is =..(f(1,2)), Parts2 is =..(g(3,4)), Combined is Parts1 & Parts2)),
    Parts1 = [f,1,2],
    Parts2 = [g,3,4],
    Combined = [f,1,2,g,3,4],
    write('  ✓ PASS: Univ combined with list append works'), nl, nl.

% Test 6: Dynamic functor creation with fixed values
test6 :-
    write('Test 6: Dynamic term construction'), nl,
    starlog_call((Term is ..=([myfunc,arg1,arg2]))),
    Term = myfunc(arg1,arg2),
    write('  ✓ PASS: Dynamic term construction works'), nl, nl.

% Test 7: Zero-argument term (atom)
test7 :-
    write('Test 7: Zero-argument term (atom)'), nl,
    starlog_call((Atom is ..=([myatom]), List is =..(Atom))),
    Atom = myatom,
    List = [myatom],
    write('  ✓ PASS: Zero-argument terms handled correctly'), nl, nl.

% Test 8: Integration with value-returning builtins
test8 :-
    write('Test 8: Integration with reverse builtin'), nl,
    starlog_call((Rev is reverse([a,b,c,d]), Term is ..=([foo,hello,world]))),
    Rev = [d,c,b,a],
    Term = foo(hello,world),
    write('  ✓ PASS: Integration with value-returning builtins works'), nl, nl.

% Test 9: Use in clause body
create_and_deconstruct(Input, Term, List) :-
    Term is ..=(Input),
    List is =..(Term).

test9 :-
    write('Test 9: Use in clause body'), nl,
    create_and_deconstruct([test,x,y,z], T, L),
    T = test(x,y,z),
    L = [test,x,y,z],
    write('  ✓ PASS: Works correctly in clause body'), nl, nl.

% Test 10: Complex real-world scenario  
complex_meta_program :-
    % Build a term dynamically from parts
    starlog_call((
        Args is [1,2,3] & [4,5],
        TermParts is [calculatesum] & Args,
        Term is ..=(TermParts),
        DeconstructedParts is =..(Term)
    )),
    Args = [1,2,3,4,5],
    TermParts = [calculatesum,1,2,3,4,5],
    Term = calculatesum(1,2,3,4,5),
    DeconstructedParts = [calculatesum,1,2,3,4,5].

test10 :-
    write('Test 10: Complex real-world metaprogramming scenario'), nl,
    complex_meta_program,
    write('  ✓ PASS: Complex metaprogramming works correctly'), nl, nl.

% Run all tests
run_all_tests :-
    write('========================================'), nl,
    write('Comprehensive Univ Operator Test Suite'), nl,
    write('========================================'), nl, nl,
    
    catch(test1, E1, (write('  ✗ FAIL: '), write(E1), nl, nl)),
    catch(test2, E2, (write('  ✗ FAIL: '), write(E2), nl, nl)),
    catch(test3, E3, (write('  ✗ FAIL: '), write(E3), nl, nl)),
    catch(test4, E4, (write('  ✗ FAIL: '), write(E4), nl, nl)),
    catch(test5, E5, (write('  ✗ FAIL: '), write(E5), nl, nl)),
    catch(test6, E6, (write('  ✗ FAIL: '), write(E6), nl, nl)),
    catch(test7, E7, (write('  ✗ FAIL: '), write(E7), nl, nl)),
    catch(test8, E8, (write('  ✗ FAIL: '), write(E8), nl, nl)),
    catch(test9, E9, (write('  ✗ FAIL: '), write(E9), nl, nl)),
    catch(test10, E10, (write('  ✗ FAIL: '), write(E10), nl, nl)),
    
    write('========================================'), nl,
    write('All tests completed successfully!'), nl,
    write('========================================'), nl.

:- initialization(run_all_tests, main).
