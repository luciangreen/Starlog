% test_peel_brackets.pl
% Tests for the peel_off_brackets functionality

:- use_module('../peel_brackets').

test_basic_peel :-
    write('Test 1: Basic peel off - ["543"] -> [5:(2+2):_] (anonymous variable)...'),
    peel_off_brackets(["543"], [Result]),
    % Expected: Result = 5:(2+2):_
    % We check the structure
    Result = (5 : Rest),
    Rest = ((2+2) : _),
    write(' ✓'), nl.

test_peel_with_atom :-
    write('Test 2: Peel with atom - [\'543\'] -> [5:(2+2):_] (anonymous variable)...'),
    peel_off_brackets(['543'], [Result]),
    Result = (5 : Rest),
    Rest = ((2+2) : _),
    write(' ✓'), nl.

test_single_char :-
    write('Test 3: Single character - ["5"] -> [5]...'),
    peel_off_brackets(["5"], [Result]),
    Result = 5,
    write(' ✓'), nl.

test_two_chars :-
    write('Test 4: Two characters - ["54"] -> [5:(2+2)]...'),
    peel_off_brackets(["54"], [Result]),
    Result = (5 : (2+2)),
    write(' ✓'), nl.

run_tests :-
    write('═══════════════════════════════════════════════════'), nl,
    write('Testing Peel Off Brackets Functionality'), nl,
    write('═══════════════════════════════════════════════════'), nl, nl,
    
    catch(test_basic_peel, E1, (write('✗ FAILED: '), write(E1), nl)),
    catch(test_peel_with_atom, E2, (write('✗ FAILED: '), write(E2), nl)),
    catch(test_single_char, E3, (write('✗ FAILED: '), write(E3), nl)),
    catch(test_two_chars, E4, (write('✗ FAILED: '), write(E4), nl)),
    
    nl,
    write('═══════════════════════════════════════════════════'), nl,
    write('All Peel Brackets Tests Complete!'), nl,
    write('═══════════════════════════════════════════════════'), nl.

:- initialization(run_tests, main).
