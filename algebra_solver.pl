% algebra_solver.pl
% Algebraic equation solver that applies operations to both sides
% Solves equations like (Y+5)/2 is 2 by isolating the variable

:- module(algebra_solver, [
    solve_equation/2,
    solve_equation/3
]).

% solve_equation(+Equation, -Solution)
% Solve an algebraic equation for the variable
% Equation is of form: Expression is Value
solve_equation(Equation, Solution) :-
    solve_equation(Equation, _, Solution).

% solve_equation(+Equation, -Variable, -Solution)
% Solve for a specific variable in the equation
% Equation: LHS is RHS where one side contains a variable
solve_equation(LHS is RHS, Variable, Solution) :-
    % Check which side has the variable
    (contains_var(LHS, Variable) ->
        solve_for_var(LHS, RHS, Variable, Solution)
    ; contains_var(RHS, Variable) ->
        solve_for_var(RHS, LHS, Variable, Solution)
    ; fail
    ).

% contains_var(+Expr, -Variable)
% Check if expression contains a variable and return it
contains_var(Var, Var) :- var(Var), !.
contains_var(Expr, Variable) :-
    compound(Expr),
    Expr =.. [_|Args],
    member(Arg, Args),
    contains_var(Arg, Variable).

% solve_for_var(+Expression, +Value, ?Variable, -Solution)
% Solve Expression = Value for Variable
% Applies inverse operations to both sides until variable is isolated

% Base case: Variable is already isolated
solve_for_var(Var, Value, Var, Solution) :-
    var(Var),
    !,
    Solution = Value.

% Handle division: (Expr)/Divisor = Value
% Apply: multiply both sides by Divisor
solve_for_var(Expr/Divisor, Value, Variable, Solution) :-
    !,
    NewValue is Value * Divisor,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle multiplication: (Expr)*Factor = Value
% Apply: divide both sides by Factor
solve_for_var(Expr*Factor, Value, Variable, Solution) :-
    \+ contains_var(Factor, Variable),
    !,
    NewValue is Value / Factor,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle multiplication (commutative): Factor*(Expr) = Value
solve_for_var(Factor*Expr, Value, Variable, Solution) :-
    \+ contains_var(Factor, Variable),
    !,
    NewValue is Value / Factor,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle addition: (Expr)+Addend = Value
% Apply: subtract Addend from both sides
solve_for_var(Expr+Addend, Value, Variable, Solution) :-
    \+ contains_var(Addend, Variable),
    !,
    NewValue is Value - Addend,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle addition (commutative): Addend+(Expr) = Value
solve_for_var(Addend+Expr, Value, Variable, Solution) :-
    \+ contains_var(Addend, Variable),
    !,
    NewValue is Value - Addend,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle subtraction: (Expr)-Subtrahend = Value
% Apply: add Subtrahend to both sides
solve_for_var(Expr-Subtrahend, Value, Variable, Solution) :-
    \+ contains_var(Subtrahend, Variable),
    !,
    NewValue is Value + Subtrahend,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle subtraction (non-commutative): Minuend-(Expr) = Value
solve_for_var(Minuend-Expr, Value, Variable, Solution) :-
    \+ contains_var(Minuend, Variable),
    !,
    NewValue is Minuend - Value,
    solve_for_var(Expr, NewValue, Variable, Solution).

% Handle power: (Expr)**Exponent = Value
% Apply: take Exponent-th root of both sides
solve_for_var(Expr**Exponent, Value, Variable, Solution) :-
    \+ contains_var(Exponent, Variable),
    !,
    NewValue is Value ** (1/Exponent),
    solve_for_var(Expr, NewValue, Variable, Solution).

% If we can't simplify further, fail
solve_for_var(Expr, _Value, _Variable, _Solution) :-
    compound(Expr),
    !,
    fail.
