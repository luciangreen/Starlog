% test_list_dual_expressions.pl
% Comprehensive tests for list dual expressions (without &)
% Tests the requirement: "Complete [A:...a] is [b:...B] and [A•a:c] is [b•B:c]. 
%                         with any combination of :,• []"

:- use_module('../starlog').

% Helper for assertions
test_case(Name, Goal) :-
    write(Name),
    write('...'),
    (starlog:starlog_call(Goal) ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, fail
    ).

% =====================================================================
% BASIC LIST DUAL EXPRESSIONS - STRING CONCATENATION
% =====================================================================

test_string_basic :-
    test_case('Basic string [A:a] is [b:B]',
        ([A:a] is [b:B], A = b, B = a)).

test_string_reversed :-
    test_case('Reversed [a:A] is [B:b]',
        ([a:A] is [B:b], A = b, B = a)).

test_string_matching_suffix :-
    test_case('Matching suffix [A:x] is [p:x]',
        ([A:x] is [p:x], A = p)).

test_string_matching_prefix :-
    test_case('Matching prefix [x:A] is [x:p]',
        ([x:A] is [x:p], A = p)).

% =====================================================================
% BASIC LIST DUAL EXPRESSIONS - ATOM CONCATENATION
% =====================================================================

test_atom_basic :-
    test_case('Basic atom [A•a] is [b•B]',
        ([A•a] is [b•B], A = b, B = a)).

test_atom_reversed :-
    test_case('Reversed [a•A] is [B•b]',
        ([a•A] is [B•b], A = b, B = a)).

test_atom_matching_suffix :-
    test_case('Matching suffix [A•x] is [p•x]',
        ([A•x] is [p•x], A = p)).

test_atom_matching_prefix :-
    test_case('Matching prefix [x•A] is [x•p]',
        ([x•A] is [x•p], A = p)).

% =====================================================================
% MIXED OPERATORS IN SINGLE ELEMENT
% =====================================================================

test_mixed_atom_string :-
    test_case('Mixed [(A•a):c] is [(b•B):c]',
        ([(A•a):c] is [(b•B):c], A = b, B = a)).

test_mixed_string_atom :-
    test_case('Mixed [(A:a)•c] is [(b:B)•c]',
        ([(A:a)•c] is [(b:B)•c], A = b, B = a)).

test_mixed_nested :-
    test_case('Nested mixed [((A•a):b):c] is [((p•a):b):c]',
        ([((A•a):b):c] is [((p•a):b):c], A = p)).

% =====================================================================
% MULTIPLE ELEMENTS IN LISTS
% =====================================================================

test_multiple_matching_suffix :-
    test_case('Multiple matching [A•q, x] is [p•q, x]',
        ([A•q, x] is [p•q, x], A = p)).

test_multiple_both_concat :-
    test_case('Multiple both concat [A•a, B•b] is [p•a, r•b]',
        ([A•a, B•b] is [p•a, r•b], A = p, B = r)).

test_multiple_string :-
    test_case('Multiple string [A:a, B:b] is [p:a, r:b]',
        ([A:a, B:b] is [p:a, r:b], A = p, B = r)).

test_multiple_mixed :-
    test_case('Multiple with literals [A•a, x, y] is [p•a, x, y]',
        ([A•a, x, y] is [p•a, x, y], A = p)).

% =====================================================================
% LONGER CONCATENATION CHAINS
% =====================================================================

test_longer_chain :-
    test_case('Longer chain [A:x:y] is [p:x:y]',
        ([A:x:y] is [p:x:y], A = p)).

test_longer_chain_atom :-
    test_case('Longer chain atom [A•x•y] is [p•x•y]',
        ([A•x•y] is [p•x•y], A = p)).

% =====================================================================
% EMPTY LISTS AND SINGLE ELEMENTS
% =====================================================================

test_single_element_string :-
    test_case('Single element string [A:a] is [b:a]',
        ([A:a] is [b:a], A = b)).

test_single_element_atom :-
    test_case('Single element atom [A•a] is [b•a]',
        ([A•a] is [b•a], A = b)).

% =====================================================================
% COMPATIBILITY WITH EXISTING FEATURES
% =====================================================================

test_compat_simple_concat :-
    test_case('Compat: simple concat still works (A:a) is (b:B)',
        ((A:a) is (b:B), A = b, B = a)).

test_compat_list_append :-
    test_case('Compat: list append still works ([1] & [2]) is [1,2]',
        (([1] & [2]) is [1,2])).

test_compat_append_with_concat :-
    test_case('Compat: append with concat ([A•b] & [d]) is [a•B, d]',
        (([A•b] & [d]) is [a•B, d], A = a, B = b)).

% =====================================================================
% EDGE CASES
% =====================================================================

test_both_bound :-
    test_case('Both sides bound [a:b] is [a:b]',
        ([a:b] is [a:b])).

test_identity_element :-
    test_case('Identity: [""] is [""]',
        ([""] is [""])).

% =====================================================================
% RUN ALL TESTS
% =====================================================================

run_tests :-
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('Testing List Dual Expressions (without &)'), nl,
    write('Problem: Complete [A:a] is [b:B] and [A•a:c] is [b•B:c]'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    
    write('=== Basic String Concatenation ==='), nl,
    catch(test_string_basic, E1, (write('✗ Error: '), write(E1), nl)),
    catch(test_string_reversed, E2, (write('✗ Error: '), write(E2), nl)),
    catch(test_string_matching_suffix, E3, (write('✗ Error: '), write(E3), nl)),
    catch(test_string_matching_prefix, E4, (write('✗ Error: '), write(E4), nl)),
    nl,
    
    write('=== Basic Atom Concatenation ==='), nl,
    catch(test_atom_basic, E5, (write('✗ Error: '), write(E5), nl)),
    catch(test_atom_reversed, E6, (write('✗ Error: '), write(E6), nl)),
    catch(test_atom_matching_suffix, E7, (write('✗ Error: '), write(E7), nl)),
    catch(test_atom_matching_prefix, E8, (write('✗ Error: '), write(E8), nl)),
    nl,
    
    write('=== Mixed Operators ==='), nl,
    catch(test_mixed_atom_string, E9, (write('✗ Error: '), write(E9), nl)),
    catch(test_mixed_string_atom, E10, (write('✗ Error: '), write(E10), nl)),
    catch(test_mixed_nested, E11, (write('✗ Error: '), write(E11), nl)),
    nl,
    
    write('=== Multiple Elements ==='), nl,
    catch(test_multiple_matching_suffix, E12, (write('✗ Error: '), write(E12), nl)),
    catch(test_multiple_both_concat, E13, (write('✗ Error: '), write(E13), nl)),
    catch(test_multiple_string, E14, (write('✗ Error: '), write(E14), nl)),
    catch(test_multiple_mixed, E15, (write('✗ Error: '), write(E15), nl)),
    nl,
    
    write('=== Longer Chains ==='), nl,
    catch(test_longer_chain, E16, (write('✗ Error: '), write(E16), nl)),
    catch(test_longer_chain_atom, E17, (write('✗ Error: '), write(E17), nl)),
    nl,
    
    write('=== Single Elements ==='), nl,
    catch(test_single_element_string, E18, (write('✗ Error: '), write(E18), nl)),
    catch(test_single_element_atom, E19, (write('✗ Error: '), write(E19), nl)),
    nl,
    
    write('=== Compatibility ==='), nl,
    catch(test_compat_simple_concat, E20, (write('✗ Error: '), write(E20), nl)),
    catch(test_compat_list_append, E21, (write('✗ Error: '), write(E21), nl)),
    catch(test_compat_append_with_concat, E22, (write('✗ Error: '), write(E22), nl)),
    nl,
    
    write('=== Edge Cases ==='), nl,
    catch(test_both_bound, E23, (write('✗ Error: '), write(E23), nl)),
    catch(test_identity_element, E24, (write('✗ Error: '), write(E24), nl)),
    nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('All List Dual Expression Tests Complete!'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(run_tests, main).
