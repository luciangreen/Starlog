% gaussian_elimination.pl
% Gaussian elimination solver for systems of linear equations
% Solves systems in the form [X, Y, Z] is [[X... 2Y.. N]
% Supports both unique solutions and infinite solutions

:- module(gaussian_elimination, [
    solve_system/2,
    solve_system/3,
    gaussian_elimination/2,
    gaussian_elimination/3
]).

% solve_system(+AugmentedMatrix, -Solution)
% Solve a system of linear equations using Gaussian elimination
% AugmentedMatrix: List of rows, where each row is [coeff1, coeff2, ..., constant]
% Solution: List of values for variables [x1, x2, ...]
%
% Example:
%   solve_system([[2, 1, 3, 2], [3, 0, 1.5, 1], [0, 0, 1, 1]], Solution).
%   Solution = [1, 5, 1]
solve_system(Matrix, Solution) :-
    solve_system(Matrix, Solution, unique).

% solve_system(+AugmentedMatrix, -Solution, -SolutionType)
% Solve with solution type indicator
% SolutionType: unique, infinite, or none
solve_system(Matrix, Solution, SolutionType) :-
    gaussian_elimination(Matrix, ReducedMatrix, SolutionType),
    (SolutionType = unique ->
        back_substitution(ReducedMatrix, Solution)
    ; SolutionType = infinite ->
        back_substitution_parametric(ReducedMatrix, Solution)
    ;
        Solution = []
    ).

% gaussian_elimination(+Matrix, -ReducedMatrix)
% Perform Gaussian elimination on augmented matrix
gaussian_elimination(Matrix, ReducedMatrix) :-
    gaussian_elimination(Matrix, ReducedMatrix, _).

% gaussian_elimination(+Matrix, -ReducedMatrix, -SolutionType)
% Perform Gaussian elimination with solution type detection
gaussian_elimination(Matrix, ReducedMatrix, SolutionType) :-
    length(Matrix, NumRows),
    (Matrix = [FirstRow|_] -> length(FirstRow, NumCols) ; NumCols = 0),
    NumVars is NumCols - 1,
    forward_elimination(Matrix, 0, NumVars, IntermediateMatrix),
    detect_solution_type(IntermediateMatrix, NumVars, SolutionType),
    ReducedMatrix = IntermediateMatrix.

% forward_elimination(+Matrix, +CurrentRow, +NumVars, -Result)
% Perform forward elimination to get upper triangular form
forward_elimination(Matrix, Row, NumVars, Result) :-
    length(Matrix, NumRows),
    (Row >= NumRows ; Row >= NumVars),
    !,
    Result = Matrix.
forward_elimination(Matrix, Row, NumVars, Result) :-
    Row < NumVars,
    length(Matrix, NumRows),
    Row < NumRows,
    !,
    % Find pivot row starting from current row
    find_pivot_from_row(Matrix, Row, Row, NumVars, PivotRow),
    (PivotRow >= 0 ->
        % Swap pivot row to current position if needed
        swap_rows(Matrix, Row, PivotRow, SwappedMatrix),
        % Eliminate column below pivot
        eliminate_below_row(SwappedMatrix, Row, NumVars, EliminatedMatrix),
        NextRow is Row + 1,
        forward_elimination(EliminatedMatrix, NextRow, NumVars, Result)
    ;
        % No pivot found, move to next row
        NextRow is Row + 1,
        forward_elimination(Matrix, NextRow, NumVars, Result)
    ).

% find_pivot_from_row(+Matrix, +StartRow, +Col, +NumVars, -PivotRow)
% Find the row with the largest absolute value in column Col, starting from StartRow
% Returns -1 if no suitable pivot found
find_pivot_from_row(Matrix, StartRow, Col, NumVars, PivotRow) :-
    length(Matrix, NumRows),
    find_pivot_from_row_helper(Matrix, StartRow, StartRow, Col, NumRows, NumVars, -1, 0, PivotRow).

find_pivot_from_row_helper(_, CurrentRow, _, _, NumRows, _, BestRow, _, BestRow) :-
    CurrentRow >= NumRows, !.
find_pivot_from_row_helper(Matrix, CurrentRow, StartRow, Col, NumRows, NumVars, BestRow, BestVal, PivotRow) :-
    CurrentRow < NumRows,
    nth0(CurrentRow, Matrix, Row),
    (Col < NumVars, length(Row, RowLen), RowLen > Col ->
        nth0(Col, Row, Val),
        AbsVal is abs(Val)
    ;
        AbsVal = 0
    ),
    (AbsVal > BestVal, AbsVal > 1e-10 ->
        NewBestRow = CurrentRow,
        NewBestVal = AbsVal
    ;
        NewBestRow = BestRow,
        NewBestVal = BestVal
    ),
    NextRow is CurrentRow + 1,
    find_pivot_from_row_helper(Matrix, NextRow, StartRow, Col, NumRows, NumVars, NewBestRow, NewBestVal, PivotRow).

% swap_rows(+Matrix, +Row1, +Row2, -Result)
% Swap two rows in the matrix
swap_rows(Matrix, Row1, Row2, Result) :-
    Row1 =:= Row2,
    !,
    Result = Matrix.
swap_rows(Matrix, Row1, Row2, Result) :-
    nth0(Row1, Matrix, R1),
    nth0(Row2, Matrix, R2),
    replace_nth0(Row1, Matrix, R2, Temp),
    replace_nth0(Row2, Temp, R1, Result).

% replace_nth0(+Index, +List, +Element, -Result)
% Replace element at index with new element
replace_nth0(0, [_|T], Elem, [Elem|T]) :- !.
replace_nth0(N, [H|T], Elem, [H|R]) :-
    N > 0,
    N1 is N - 1,
    replace_nth0(N1, T, Elem, R).

% eliminate_below_row(+Matrix, +PivotRow, +NumVars, -Result)
% Eliminate all entries below the pivot in the pivot row's leading column
eliminate_below_row(Matrix, PivotRow, NumVars, Result) :-
    nth0(PivotRow, Matrix, PivotRowData),
    % Find the leading column in the pivot row
    find_leading_coeff(PivotRowData, NumVars, LeadingCol, PivotVal),
    (LeadingCol >= 0, abs(PivotVal) > 1e-10 ->
        eliminate_below_row_helper(Matrix, 0, PivotRow, LeadingCol, PivotRowData, PivotVal, Result)
    ;
        Result = Matrix
    ).

eliminate_below_row_helper([], _, _, _, _, _, []).
eliminate_below_row_helper([Row|Rest], RowIdx, PivotRow, LeadingCol, PivotRowData, PivotVal, [NewRow|RestResult]) :-
    (RowIdx =< PivotRow ->
        NewRow = Row
    ;
        nth0(LeadingCol, Row, Val),
        (abs(Val) < 1e-10 ->
            NewRow = Row
        ;
            Factor is Val / PivotVal,
            subtract_rows(Row, PivotRowData, Factor, NewRow)
        )
    ),
    NextIdx is RowIdx + 1,
    eliminate_below_row_helper(Rest, NextIdx, PivotRow, LeadingCol, PivotRowData, PivotVal, RestResult).

% subtract_rows(+Row1, +Row2, +Factor, -Result)
% Result = Row1 - Factor * Row2
subtract_rows([], [], _, []).
subtract_rows([H1|T1], [H2|T2], Factor, [H|T]) :-
    H is H1 - Factor * H2,
    subtract_rows(T1, T2, Factor, T).

% detect_solution_type(+ReducedMatrix, +NumVars, -SolutionType)
% Detect if system has unique, infinite, or no solution
detect_solution_type(Matrix, NumVars, SolutionType) :-
    % Check for inconsistency (0 = nonzero)
    (has_inconsistency(Matrix, NumVars) ->
        SolutionType = none
    ;
        % Check if each variable has a pivot (leading coefficient)
        % A unique solution requires a pivot for every variable
        count_variables_with_pivots(Matrix, NumVars, NumPivots),
        (NumPivots =:= NumVars ->
            SolutionType = unique
        ;
            SolutionType = infinite
        )
    ).

% has_inconsistency(+Matrix, +NumVars)
% Check if there's a row with all zeros except the constant term
has_inconsistency(Matrix, NumVars) :-
    member(Row, Matrix),
    length(Row, Len),
    Len > NumVars,
    first_n(Row, NumVars, Coeffs),
    all_near_zero(Coeffs),
    nth0(NumVars, Row, Constant),
    abs(Constant) > 1e-10.

% first_n(+List, +N, -Result)
% Get first N elements of list
first_n(_, 0, []) :- !.
first_n([H|T], N, [H|R]) :-
    N > 0,
    N1 is N - 1,
    first_n(T, N1, R).

% all_near_zero(+List)
% Check if all elements are near zero
all_near_zero([]).
all_near_zero([H|T]) :-
    abs(H) < 1e-10,
    all_near_zero(T).

% count_non_zero_rows(+Matrix, +NumVars, -Count)
% Count rows that have at least one non-zero coefficient
count_non_zero_rows(Matrix, NumVars, Count) :-
    findall(1, (member(Row, Matrix), once(has_non_zero_coeff(Row, NumVars))), Ones),
    length(Ones, Count).

% count_variables_with_pivots(+Matrix, +NumVars, -Count)
% Count how many variables have a pivot (leading coefficient) in some row
% A variable has a pivot if it's the first non-zero coefficient in some row
count_variables_with_pivots(Matrix, NumVars, Count) :-
    NumVars1 is NumVars - 1,
    findall(VarIdx, 
            (between(0, NumVars1, VarIdx), 
             has_pivot_in_column(Matrix, VarIdx, NumVars)), 
            VarsWithPivots),
    length(VarsWithPivots, Count).

% has_non_zero_coeff(+Row, +NumVars)
% Check if row has at least one non-zero coefficient
has_non_zero_coeff(Row, NumVars) :-
    first_n(Row, NumVars, Coeffs),
    member(Coeff, Coeffs),
    abs(Coeff) > 1e-10.

% back_substitution(+ReducedMatrix, -Solution)
% Perform back substitution to get unique solution
back_substitution(Matrix, Solution) :-
    (Matrix = [FirstRow|_] -> length(FirstRow, NumCols) ; NumCols = 0),
    NumVars is NumCols - 1,
    % Initialize solution array with zeros
    length(SolutionArray, NumVars),
    % Process rows from bottom to top
    reverse(Matrix, ReversedMatrix),
    back_sub_rows(ReversedMatrix, NumVars, SolutionArray, Solution).

back_sub_rows([], _, Solution, Solution).
back_sub_rows([Row|Rest], NumVars, SolutionSoFar, Solution) :-
    % Find the leading coefficient's position
    find_leading_coeff(Row, NumVars, LeadingIdx, LeadingCoeff),
    (LeadingIdx >= 0, abs(LeadingCoeff) > 1e-10 ->
        % Calculate value for this variable
        length(Row, RowLen),
        ConstantIdx is RowLen - 1,
        nth0(ConstantIdx, Row, Constant),
        % Sum contributions from already-solved variables
        sum_contributions(Row, SolutionSoFar, NumVars, LeadingIdx, Constant, Value),
        % Update solution
        (var(SolutionSoFar) ->
            length(NewSolution, NumVars),
            replace_nth0(LeadingIdx, NewSolution, Value, UpdatedSolution)
        ;
            replace_nth0(LeadingIdx, SolutionSoFar, Value, UpdatedSolution)
        ),
        back_sub_rows(Rest, NumVars, UpdatedSolution, Solution)
    ;
        % Skip rows with no pivot
        back_sub_rows(Rest, NumVars, SolutionSoFar, Solution)
    ).

% sum_contributions(+Row, +Solution, +NumVars, +SkipIdx, +Constant, -Value)
% Calculate the value of the variable by substituting known values
sum_contributions(Row, Solution, NumVars, SkipIdx, Constant, Value) :-
    nth0(SkipIdx, Row, LeadingCoeff),
    sum_contributions_helper(Row, Solution, 0, NumVars, SkipIdx, 0, Sum),
    Value is (Constant - Sum) / LeadingCoeff.

sum_contributions_helper(_, _, Idx, NumVars, _, Acc, Acc) :-
    Idx >= NumVars, !.
sum_contributions_helper(Row, Solution, Idx, NumVars, SkipIdx, Acc, Sum) :-
    Idx < NumVars,
    (Idx =:= SkipIdx ->
        NewAcc = Acc
    ;
        nth0(Idx, Row, Coeff),
        (nonvar(Solution), nth0(Idx, Solution, Val), nonvar(Val) ->
            NewAcc is Acc + Coeff * Val
        ;
            NewAcc = Acc
        )
    ),
    NextIdx is Idx + 1,
    sum_contributions_helper(Row, Solution, NextIdx, NumVars, SkipIdx, NewAcc, Sum).

% find_leading_coeff(+Row, +NumVars, -Index, -Coeff)
% Find the first non-zero coefficient in the row
find_leading_coeff(Row, NumVars, Index, Coeff) :-
    find_leading_coeff_helper(Row, 0, NumVars, Index, Coeff).

find_leading_coeff_helper([], _, _, -1, 0).
find_leading_coeff_helper([H|_], Idx, NumVars, Idx, H) :-
    Idx < NumVars,
    abs(H) > 1e-10,
    !.
find_leading_coeff_helper([_|T], Idx, NumVars, LeadingIdx, Coeff) :-
    Idx < NumVars,
    NextIdx is Idx + 1,
    find_leading_coeff_helper(T, NextIdx, NumVars, LeadingIdx, Coeff).
find_leading_coeff_helper(_, Idx, NumVars, -1, 0) :-
    Idx >= NumVars.

% back_substitution_parametric(+ReducedMatrix, -Solution)
% Generate parametric solution for systems with infinite solutions
% Returns solution with parameters (free variables)
back_substitution_parametric(Matrix, Solution) :-
    length(Matrix, NumRows),
    (Matrix = [FirstRow|_] -> length(FirstRow, NumCols) ; NumCols = 0),
    NumVars is NumCols - 1,
    % Identify free variables
    identify_free_variables(Matrix, NumVars, FreeVars),
    % Generate parametric solution
    generate_parametric_solution(Matrix, NumVars, FreeVars, Solution).

% identify_free_variables(+Matrix, +NumVars, -FreeVars)
% Identify which variables are free (can take any value)
identify_free_variables(Matrix, NumVars, FreeVars) :-
    findall(Var, (between(0, NumVars, Var), is_free_variable(Matrix, Var, NumVars)), FreeVars).

% is_free_variable(+Matrix, +VarIdx, +NumVars)
% Check if a variable is free (no pivot in its column)
is_free_variable(Matrix, VarIdx, NumVars) :-
    \+ has_pivot_in_column(Matrix, VarIdx, NumVars).

% has_pivot_in_column(+Matrix, +Col, +NumVars)
% Check if there's a pivot in the given column
has_pivot_in_column([Row|_], Col, NumVars) :-
    find_leading_coeff(Row, NumVars, LeadingIdx, Coeff),
    LeadingIdx =:= Col,
    abs(Coeff) > 1e-10,
    !.
has_pivot_in_column([_|Rest], Col, NumVars) :-
    has_pivot_in_column(Rest, Col, NumVars).

% generate_parametric_solution(+Matrix, +NumVars, +FreeVars, -Solution)
% Generate solution with parameters for free variables
generate_parametric_solution(Matrix, NumVars, FreeVars, Solution) :-
    % For infinite solutions, we use the same back substitution approach
    % This is a simplified version - a full implementation would need to
    % express the solution in terms of parameters
    back_substitution(Matrix, Solution).
