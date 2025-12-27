% test_univ.pl
% Tests for univ operator (=..) support in Starlog

:- use_module('../starlog').

% Test list to term conversion: A is ..=([f,0,1])
test_list_to_term :-
    starlog_call((A is ..=([f,0,1]))),
    A = f(0,1),
    write('✓ List to term conversion test passed'), nl.

% Test term to list conversion: A is =..(f(0,1))
test_term_to_list :-
    starlog_call((A is =..(f(0,1)))),
    A = [f,0,1],
    write('✓ Term to list conversion test passed'), nl.

% Test with different functors
test_different_functor :-
    starlog_call((B is ..=([foo,a,b,c]))),
    B = foo(a,b,c),
    write('✓ Different functor test passed'), nl.

% Test with nullary functor
test_nullary_functor :-
    starlog_call((C is ..=([atom]))),
    C = atom,
    write('✓ Nullary functor test passed'), nl.

% Test roundtrip: list -> term -> list
test_roundtrip :-
    starlog_call((T is ..=([g,1,2]), L is =..(T))),
    L = [g,1,2],
    write('✓ Roundtrip test passed'), nl.

% Test in clause body
test_in_clause :-
    test_clause_helper(Result),
    Result = bar(x,y),
    write('✓ In clause body test passed'), nl.

test_clause_helper(R) :- R is ..=([bar,x,y]).

% Test term to list in clause body
test_term_to_list_in_clause :-
    test_term_helper(L),
    L = [baz,1,2,3],
    write('✓ Term to list in clause body test passed'), nl.

test_term_helper(L) :- L is =..(baz(1,2,3)).

% Run all tests
run_tests :-
    write('Running univ operator tests...'), nl, nl,
    catch(test_list_to_term, E1, (write('✗ List to term failed: '), write(E1), nl)),
    catch(test_term_to_list, E2, (write('✗ Term to list failed: '), write(E2), nl)),
    catch(test_different_functor, E3, (write('✗ Different functor failed: '), write(E3), nl)),
    catch(test_nullary_functor, E4, (write('✗ Nullary functor failed: '), write(E4), nl)),
    catch(test_roundtrip, E5, (write('✗ Roundtrip failed: '), write(E5), nl)),
    catch(test_in_clause, E6, (write('✗ In clause body failed: '), write(E6), nl)),
    catch(test_term_to_list_in_clause, E7, (write('✗ Term to list in clause failed: '), write(E7), nl)),
    nl,
    write('Univ tests complete!'), nl.

:- initialization(run_tests, main).
