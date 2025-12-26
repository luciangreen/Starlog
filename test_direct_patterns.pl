:- use_module(starlog_in_prolog).

% Direct test of the patterns from the problem statement

test_pattern_1 :-
    write('Pattern 1: C is [1]&A, C is B&[2]'), nl,
    % This should unify: [1]&A = B&[2]
    C is [1] & A,
    C is B & [2],
    write('  A = '), write(A), nl,
    write('  B = '), write(B), nl,
    write('  C = '), write(C), nl.

test_pattern_2 :-
    write('Pattern 2: 3 is 1 Op 2 where Op in {+,-,/}'), nl,
    % Try each operator
    findall(Op, (member(Op, [+,-,/]), Expr =.. [Op, 1, 2], 3 is Expr), Ops),
    write('  Valid operators: '), write(Ops), nl.

test_pattern_3 :-
    write('Pattern 3: 3 is 1+A where A in {1,2,3}'), nl,
    % Find A such that 3 = 1+A
    findall(A, (member(A, [1,2,3]), 3 is 1+A), As),
    write('  Valid values for A: '), write(As), nl.

test_pattern_4 :-
    write('Pattern 4: A is (B is 1)+B'), nl,
    % This should be: B=1, then A = 1+1 = 2
    B is 1,
    A is B+B,
    write('  B = '), write(B), nl,
    write('  A = '), write(A), nl.

run_all :-
    write('Direct pattern tests:'), nl, nl,
    catch(test_pattern_1, E1, (write('Error in pattern 1: '), write(E1), nl)), nl,
    catch(test_pattern_2, E2, (write('Error in pattern 2: '), write(E2), nl)), nl,
    catch(test_pattern_3, E3, (write('Error in pattern 3: '), write(E3), nl)), nl,
    catch(test_pattern_4, E4, (write('Error in pattern 4: '), write(E4), nl)), nl.

:- initialization(run_all, main).
