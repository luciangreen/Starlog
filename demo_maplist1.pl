% demo_maplist1.pl
% Demonstration of maplist1 predicate usage

:- use_module(starlog).

% Example 1: Using maplist1 to unify all elements with a free variable
demo_maplist1_unify_free :-
    write('═══════════════════════════════════════════'), nl,
    write('Example 1: maplist1(=(_),[1,1,1])'), nl,
    write('  Unifies each element with a free variable'), nl,
    (maplist1(=(_),[1,1,1]) ->
        write('  Result: SUCCESS ✓') 
    ;
        write('  Result: FAILED ✗')
    ), nl, nl.

% Example 2: Using maplist1 to check all elements are equal to 1
demo_maplist1_unify_value :-
    write('Example 2: maplist1(=(1),[1,1,1])'), nl,
    write('  Checks if all elements equal 1'), nl,
    (maplist1(=(1),[1,1,1]) ->
        write('  Result: SUCCESS ✓') 
    ;
        write('  Result: FAILED ✗')
    ), nl, nl.

% Example 3: Using maplist1 with atom type checking
demo_maplist1_atom :-
    write('Example 3: maplist1(atom,[a,b,c])'), nl,
    write('  Checks if all elements are atoms'), nl,
    (maplist1(atom,[a,b,c]) ->
        write('  Result: SUCCESS ✓') 
    ;
        write('  Result: FAILED ✗')
    ), nl, nl.

% Example 4: Using maplist1 with number type checking
demo_maplist1_number :-
    write('Example 4: maplist1(number,[1,2,3])'), nl,
    write('  Checks if all elements are numbers'), nl,
    (maplist1(number,[1,2,3]) ->
        write('  Result: SUCCESS ✓') 
    ;
        write('  Result: FAILED ✗')
    ), nl, nl.

% Example 5: Failure case
demo_maplist1_fail :-
    write('Example 5: maplist1(=(1),[1,2,3])'), nl,
    write('  Should fail - not all elements are 1'), nl,
    (maplist1(=(1),[1,2,3]) ->
        write('  Result: UNEXPECTED SUCCESS ✗') 
    ;
        write('  Result: CORRECTLY FAILED ✓')
    ), nl, nl.

% Example 6: Using with a compound goal to bind a variable
demo_maplist1_compound :-
    write('Example 6: maplist1(=(X),[5,5,5]), X = 5'), nl,
    write('  Binds X to the common value of all elements'), nl,
    (maplist1(=(X),[5,5,5]), X = 5 ->
        write('  Result: SUCCESS, X = '), write(X), write(' ✓') 
    ;
        write('  Result: FAILED ✗')
    ), nl, nl.

% Run all demos
run_demos :-
    nl,
    write('═══════════════════════════════════════════'), nl,
    write('  MAPLIST1 DEMONSTRATION'), nl,
    write('═══════════════════════════════════════════'), nl, nl,
    
    demo_maplist1_unify_free,
    demo_maplist1_unify_value,
    demo_maplist1_atom,
    demo_maplist1_number,
    demo_maplist1_fail,
    demo_maplist1_compound,
    
    write('═══════════════════════════════════════════'), nl,
    write('  ALL DEMOS COMPLETE'), nl,
    write('═══════════════════════════════════════════'), nl.

% Auto-run demos when loaded
:- initialization(run_demos, main).
