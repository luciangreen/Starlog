% test_new_pattern.pl
% Test the new pattern: ([A•b] & [d]) is [a•B, d] and all configurations

:- use_module(starlog).

test_basic_pattern :-
    write('Test 1: ([A•b] & [d]) is [a•B, d]'), nl,
    catch(
        (
            (([A•b] & [d]) is [a•B, d]),
            format('  A = ~w, B = ~w~n', [A, B]),
            % Verify
            atom_concat(A, b, AB),
            atom_concat(a, B, AB2),
            (AB = AB2 -> write('  ✓ Verified: A•b = a•B') ; write('  ✗ Verification failed')), nl
        ),
        Error,
        (
            format('  ✗ Error: ~w~n', [Error]),
            write('  This pattern needs to be implemented'), nl
        )
    ).

test_string_version :-
    write('Test 2: ([A:"b"] & ["d"]) is ["a":B, "d"] (string version)'), nl,
    catch(
        (
            (([A:"b"] & ["d"]) is ["a":B, "d"]),
            format('  A = ~w, B = ~w~n', [A, B]),
            string_concat(A, "b", AB),
            string_concat("a", B, AB2),
            (AB = AB2 -> write('  ✓ Verified: A:"b" = "a":B') ; write('  ✗ Verification failed')), nl
        ),
        Error,
        (
            format('  ✗ Error: ~w~n', [Error]),
            write('  This pattern needs to be implemented'), nl
        )
    ).

test_simpler_case :-
    write('Test 3: Simpler case without & - (A•b) is (a•B)'), nl,
    catch(
        (
            ((A•b) is (a•B)),
            format('  A = ~w, B = ~w~n', [A, B]),
            write('  ✓ Simple dual concat works'), nl
        ),
        Error,
        (
            format('  ✗ Error: ~w~n', [Error]), nl
        )
    ).

test_list_with_concat_lhs_only :-
    write('Test 4: LHS only has concat - ([A•b] & [d]) is [ab, d]'), nl,
    catch(
        (
            (([A•b] & [d]) is [ab, d]),
            format('  A = ~w~n', [A]),
            atom_concat(A, b, ab),
            write('  ✓ LHS concat with RHS plain value works'), nl
        ),
        Error,
        (
            format('  ✗ Error: ~w~n', [Error]), nl
        )
    ).

main :-
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('Testing Problem Statement: ([A•b] & [d]) is [a•B, d]'), nl,
    write('And all configurations and combinations'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl, nl,
    
    test_basic_pattern,
    nl,
    test_string_version,
    nl,
    test_simpler_case,
    nl,
    test_list_with_concat_lhs_only,
    nl,
    
    write('═══════════════════════════════════════════════════════════════'), nl,
    write('Test sequence complete'), nl,
    write('═══════════════════════════════════════════════════════════════'), nl.

:- initialization(main, main).
