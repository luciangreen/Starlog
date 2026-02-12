% test_maplist1.pl
% Tests for maplist1 implementation

:- use_module('../starlog').

% Test 1: Basic unification with =(_)
% This is the example from the problem statement
test_maplist1_unify_free :-
    maplist1(=(_), [1,1,1]),
    write('✓ test_maplist1_unify_free passed'), nl.

% Test 2: Unification with a specific value
test_maplist1_unify_value :-
    maplist1(=(1), [1,1,1]),
    write('✓ test_maplist1_unify_value passed'), nl.

% Test 3: Check if all elements are atoms
test_maplist1_atom :-
    maplist1(atom, [a, b, c]),
    write('✓ test_maplist1_atom passed'), nl.

% Test 4: Check if all elements are numbers
test_maplist1_number :-
    maplist1(number, [1, 2, 3]),
    write('✓ test_maplist1_number passed'), nl.

% Test 5: Empty list
test_maplist1_empty :-
    maplist1(atom, []),
    write('✓ test_maplist1_empty passed'), nl.

% Test 6: Failing case - not all elements match
test_maplist1_fail :-
    \+ maplist1(=(1), [1,2,3]),
    write('✓ test_maplist1_fail passed'), nl.

% Test 7: Using a compound goal
test_maplist1_compound :-
    maplist1(=(X), [5,5,5]),
    X = 5,
    write('✓ test_maplist1_compound passed'), nl.

% Run all tests
run_tests :-
    write('═══════════════════════════════════════════'), nl,
    write('  MAPLIST1 TESTS'), nl,
    write('═══════════════════════════════════════════'), nl, nl,
    
    catch(test_maplist1_unify_free, E1, 
        (write('✗ test_maplist1_unify_free failed: '), write(E1), nl)),
    catch(test_maplist1_unify_value, E2, 
        (write('✗ test_maplist1_unify_value failed: '), write(E2), nl)),
    catch(test_maplist1_atom, E3, 
        (write('✗ test_maplist1_atom failed: '), write(E3), nl)),
    catch(test_maplist1_number, E4, 
        (write('✗ test_maplist1_number failed: '), write(E4), nl)),
    catch(test_maplist1_empty, E5, 
        (write('✗ test_maplist1_empty failed: '), write(E5), nl)),
    catch(test_maplist1_fail, E6, 
        (write('✗ test_maplist1_fail failed: '), write(E6), nl)),
    catch(test_maplist1_compound, E7, 
        (write('✗ test_maplist1_compound failed: '), write(E7), nl)),
    
    nl,
    write('═══════════════════════════════════════════'), nl,
    write('  ALL TESTS COMPLETE'), nl,
    write('═══════════════════════════════════════════'), nl.

% Auto-run tests when loaded
:- initialization(run_tests, main).
