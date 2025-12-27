% test_list_append_concat_constraints.pl
% Tests for list append expressions with concat constraints
% Problem statement: "Complete [A:4]&[3] is ["54",3]., A&[3] is ["54",3]. without hanging, 
%                     [5:A]&[3] is ["54",3]. and all combinations and configurations 
%                     with variables on the left and right-hand sides."

:- use_module('../starlog').

% Test helper
test_case(Name, Goal) :-
    write(Name),
    write('...'),
    (starlog:starlog_call(Goal) ->
        write(' ✓'), nl
    ;
        write(' ✗ FAILED'), nl, fail
    ).

% Core patterns from problem statement
test_core_1 :-
    test_case('Core 1: [A:4]&[3] is ["54",3]',
        (([A:4] & [3]) is ["54",3], A = "5")).

test_core_2 :-
    test_case('Core 2: A&[3] is ["54",3] (without hanging)',
        ((A & [3]) is ["54",3], A = ["54"])).

test_core_3 :-
    test_case('Core 3: [5:A]&[3] is ["54",3]',
        (([5:A] & [3]) is ["54",3], A = "4")).

% String concatenation variations - first position variable
test_string_first_pos_1 :-
    test_case('String first pos 1: [A:"4"]&[3] is ["54",3]',
        (([A:"4"] & [3]) is ["54",3], A = "5")).

test_string_first_pos_2 :-
    test_case('String first pos 2: [A:b]&[c] is ["ab",c]',
        (([A:b] & [c]) is ["ab",c], A = "a")).

test_string_first_pos_3 :-
    test_case('String first pos 3: [A:"hello"]&[x] is ["worldhello",x]',
        (([A:"hello"] & [x]) is ["worldhello",x], A = "world")).

% String concatenation variations - second position variable
test_string_second_pos_1 :-
    test_case('String second pos 1: ["5":A]&[3] is ["54",3]',
        ((["5":A] & [3]) is ["54",3], A = "4")).

test_string_second_pos_2 :-
    test_case('String second pos 2: [a:B]&[c] is ["ab",c]',
        (([a:B] & [c]) is ["ab",c], B = "b")).

test_string_second_pos_3 :-
    test_case('String second pos 3: ["hello":A]&[x] is ["helloworld",x]',
        ((["hello":A] & [x]) is ["helloworld",x], A = "world")).

% Atom concatenation variations
test_atom_first_pos :-
    test_case('Atom first pos: [A•world]&[x] is [helloworld,x]',
        (([A•world] & [x]) is [helloworld,x], A = hello)).

test_atom_second_pos :-
    test_case('Atom second pos: [hello•A]&[x] is [helloworld,x]',
        (([hello•A] & [x]) is [helloworld,x], A = world)).

% Multiple elements with concat
test_multiple_1 :-
    test_case('Multiple 1: [A:4,7]&[3] is ["54",7,3]',
        (([A:4,7] & [3]) is ["54",7,3], A = "5")).

test_multiple_2 :-
    test_case('Multiple 2: [5:A,7]&[3] is ["54",7,3]',
        (([5:A,7] & [3]) is ["54",7,3], A = "4")).

test_multiple_3 :-
    test_case('Multiple 3: ["5":"4",7]&[3] is ["54",7,3]',
        ((["5":"4",7] & [3]) is ["54",7,3])).

% Variable on right-hand side of append
test_rhs_var_1 :-
    test_case('RHS var 1: [A:4]&B is ["54",3]',
        (([A:4] & B) is ["54",3], A = "5", B = [3])).

test_rhs_var_2 :-
    test_case('RHS var 2: [5:A]&B is ["54",3]',
        (([5:A] & B) is ["54",3], A = "4", B = [3])).

test_rhs_var_3 :-
    test_case('RHS var 3: ["5":A]&B is ["54",3]',
        ((["5":A] & B) is ["54",3], A = "4", B = [3])).

% Nested concatenation
test_nested_1 :-
    test_case('Nested 1: [5:4:A]&[3] is ["543",3]',
        (([5:4:A] & [3]) is ["543",3], A = "3")).

test_nested_2 :-
    test_case('Nested 2: ["5":"4":"3"]&[3] is ["543",3]',
        ((["5":"4":"3"] & [3]) is ["543",3])).

test_nested_3 :-
    test_case('Nested 3: [5:"43"]&[3] is ["543",3]',
        (([5:"43"] & [3]) is ["543",3])).

% Variable as entire left list
test_left_list_var_1 :-
    test_case('Left list var 1: A&[3] is ["54",3]',
        ((A & [3]) is ["54",3], A = ["54"])).

test_left_list_var_2 :-
    test_case('Left list var 2: A&[x,y] is [a,b,x,y]',
        ((A & [x,y]) is [a,b,x,y], A = [a,b])).

test_left_list_var_3 :-
    test_case('Left list var 3: A&B is [1,2,3]',
        ((A & B) is [1,2,3], (A=[1,2,3], B=[] ; A=[1,2], B=[3] ; A=[1], B=[2,3] ; A=[], B=[1,2,3]))).

% Variable as entire right list
test_right_list_var_1 :-
    test_case('Right list var 1: ["54"]&A is ["54",3]',
        ((["54"] & A) is ["54",3], A = [3])).

test_right_list_var_2 :-
    test_case('Right list var 2: [a,b]&A is [a,b,x,y]',
        (([a,b] & A) is [a,b,x,y], A = [x,y])).

% Complex combinations
test_complex_1 :-
    test_case('Complex 1: [A:"4"]&[3] is ["54",3]',
        (([A:"4"] & [3]) is ["54",3], A = "5")).

test_complex_2 :-
    test_case('Complex 2: [5:A]&[3] is ["54",3]',
        (([5:A] & [3]) is ["54",3], A = "4")).

test_complex_3 :-
    test_case('Complex 3: [A•b]&[d] is [ab,d]',
        (([A•b] & [d]) is [ab,d], A = a)).

% From original problem statement integration test
test_integration_original :-
    test_case('Integration: [5:(2+2)]&A is ["54",3]',
        (([5:(2+2)] & A) is ["54",3], A = [3])).

% Edge cases
test_edge_empty_right :-
    test_case('Edge case: [A:4]&[] is ["54"]',
        (([A:4] & []) is ["54"], A = "5")).

test_edge_empty_left :-
    test_case('Edge case: []&[3] is [3]',
        (([] & [3]) is [3])).

run_tests :-
    write('==================================================================='), nl,
    write('Testing List Append with Concat Constraints'), nl,
    write('Problem: Complete [A:4]&[3] is ["54",3], A&[3] is ["54",3],'), nl,
    write('         [5:A]&[3] is ["54",3] and all combinations'), nl,
    write('==================================================================='), nl, nl,
    
    write('=== Core Patterns from Problem Statement ==='), nl,
    catch(test_core_1, E1, (write('✗ Error: '), write(E1), nl)),
    catch(test_core_2, E2, (write('✗ Error: '), write(E2), nl)),
    catch(test_core_3, E3, (write('✗ Error: '), write(E3), nl)),
    nl,
    
    write('=== String Concatenation - First Position Variable ==='), nl,
    catch(test_string_first_pos_1, E4, (write('✗ Error: '), write(E4), nl)),
    catch(test_string_first_pos_2, E5, (write('✗ Error: '), write(E5), nl)),
    catch(test_string_first_pos_3, E6, (write('✗ Error: '), write(E6), nl)),
    nl,
    
    write('=== String Concatenation - Second Position Variable ==='), nl,
    catch(test_string_second_pos_1, E7, (write('✗ Error: '), write(E7), nl)),
    catch(test_string_second_pos_2, E8, (write('✗ Error: '), write(E8), nl)),
    catch(test_string_second_pos_3, E9, (write('✗ Error: '), write(E9), nl)),
    nl,
    
    write('=== Atom Concatenation ==='), nl,
    catch(test_atom_first_pos, E10, (write('✗ Error: '), write(E10), nl)),
    catch(test_atom_second_pos, E11, (write('✗ Error: '), write(E11), nl)),
    nl,
    
    write('=== Multiple Elements with Concat ==='), nl,
    catch(test_multiple_1, E12, (write('✗ Error: '), write(E12), nl)),
    catch(test_multiple_2, E13, (write('✗ Error: '), write(E13), nl)),
    catch(test_multiple_3, E14, (write('✗ Error: '), write(E14), nl)),
    nl,
    
    write('=== Variable on Right-Hand Side of Append ==='), nl,
    catch(test_rhs_var_1, E15, (write('✗ Error: '), write(E15), nl)),
    catch(test_rhs_var_2, E16, (write('✗ Error: '), write(E16), nl)),
    catch(test_rhs_var_3, E17, (write('✗ Error: '), write(E17), nl)),
    nl,
    
    write('=== Nested Concatenation ==='), nl,
    catch(test_nested_1, E18, (write('✗ Error: '), write(E18), nl)),
    catch(test_nested_2, E19, (write('✗ Error: '), write(E19), nl)),
    catch(test_nested_3, E20, (write('✗ Error: '), write(E20), nl)),
    nl,
    
    write('=== Variable as Entire List ==='), nl,
    catch(test_left_list_var_1, E21, (write('✗ Error: '), write(E21), nl)),
    catch(test_left_list_var_2, E22, (write('✗ Error: '), write(E22), nl)),
    catch(test_left_list_var_3, E23, (write('✗ Error: '), write(E23), nl)),
    catch(test_right_list_var_1, E24, (write('✗ Error: '), write(E24), nl)),
    catch(test_right_list_var_2, E25, (write('✗ Error: '), write(E25), nl)),
    nl,
    
    write('=== Complex Combinations ==='), nl,
    catch(test_complex_1, E26, (write('✗ Error: '), write(E26), nl)),
    catch(test_complex_2, E27, (write('✗ Error: '), write(E27), nl)),
    catch(test_complex_3, E28, (write('✗ Error: '), write(E28), nl)),
    nl,
    
    write('=== Integration and Edge Cases ==='), nl,
    catch(test_integration_original, E29, (write('✗ Error: '), write(E29), nl)),
    catch(test_edge_empty_right, E30, (write('✗ Error: '), write(E30), nl)),
    catch(test_edge_empty_left, E31, (write('✗ Error: '), write(E31), nl)),
    nl,
    
    write('==================================================================='), nl,
    write('All List Append Concat Constraint Tests Complete!'), nl,
    write('==================================================================='), nl.

:- initialization(run_tests, main).
