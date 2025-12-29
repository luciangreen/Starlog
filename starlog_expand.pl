% starlog_expand.pl
% Expansion mechanisms for Starlog-in-Prolog
% Provides goal_expansion and term_expansion to compile Starlog syntax
% into equivalent Prolog goals at load-time or goal-call-time.

:- module(starlog_expand, [
    expand_starlog_goal/2,
    expand_starlog_term/2,
    set_starlog_debug/1,
    compile_starlog_expr/3  % Export for debugging
]).

:- use_module(starlog_registry).

% Define operators locally for pattern matching
:- op(700, xfx, is).
:- op(600, yfx, ':').
:- op(600, yfx, '&').
:- op(600, yfx, '•').
:- op(600, fx, '..=').  % List to term conversion
:- op(600, fx, '=..').  % Term to list conversion (unary prefix)

% Debug flag
:- dynamic starlog_debug/1.
starlog_debug(false).

% Enable/disable debug output
set_starlog_debug(Flag) :-
    retractall(starlog_debug(_)),
    assertz(starlog_debug(Flag)).

% expand_starlog_goal(+Goal, -ExpandedGoal)
% Main entry point for expanding a single Starlog goal into Prolog goals.
expand_starlog_goal(Goal, Expanded) :-
    starlog_debug(Debug),
    (Debug = true -> format('Expanding goal: ~w~n', [Goal]) ; true),
    expand_goal_internal(Goal, Expanded),
    (Debug = true -> format('Expanded to: ~w~n', [Expanded]) ; true).

% expand_starlog_term(+Term, -ExpandedTerm)
% Entry point for term expansion (whole clauses).
expand_starlog_term((Head :- Body), (Head :- ExpandedBody)) :-
    !,
    expand_goal_internal(Body, ExpandedBody).
expand_starlog_term(Term, Term).

% expand_goal_internal(+Goal, -Expanded)
% Internal goal expansion logic.

% Handle variables - don't expand them
expand_goal_internal(Goal, Goal) :-
    var(Goal),
    !.

% Special pattern: Var=(StarlogGoal),Var -> Var=(StarlogGoal),starlog_call(Var)
% or Var=(StarlogGoal),(Var,Rest) -> Var=(StarlogGoal),(starlog_call(Var),Rest)
% This allows A=(C is no_eval(eval(1+1))),A to work correctly
expand_goal_internal((A, B), Expanded) :-
    nonvar(A),
    compound(A),
    A = (Var = RHS),  % Check if A is a unification
    var(Var),
    nonvar(RHS),
    contains_starlog_in_term(RHS),
    is_var_or_starts_with_var(B, Var),
    !,
    % Transform to use starlog_call
    expand_goal_internal(A, EA),
    replace_first_var_with_starlog_call(B, Var, NewB),
    expand_goal_internal(NewB, EB),
    Expanded = (EA, EB).

% Special pattern: Var1=(StarlogExpr),(Var2 is Var1) -> Var1=(StarlogExpr),starlog_eval(Var1,Var2)
% This allows A=([a]&[c]),B is A to work correctly
% StarlogExpr is a bare Starlog expression (not wrapped in 'is')
% Handles both: Var1=(Expr),(Var2 is Var1) and Var1=(Expr),(Var2 is Var1, Rest)
expand_goal_internal((A, B), Expanded) :-
    nonvar(A),
    compound(A),
    A = (Var1 = RHS),  % Check if A is a unification
    var(Var1),
    nonvar(RHS),
    contains_starlog_operator(RHS),  % RHS contains Starlog operators
    \+ contains_starlog_in_term(RHS),  % But is not a complete Starlog goal
    nonvar(B),
    % B could be (Var2 is Var1) or ((Var2 is Var1), Rest)
    ( compound(B), B = (IsGoal, Rest) -> 
        FirstGoal = IsGoal
    ; 
        FirstGoal = B,
        Rest = true
    ),
    compound(FirstGoal),
    FirstGoal = (Var2 is VarRef),  % First goal is of the form (Var2 is VarRef)
    Var1 == VarRef,  % Ensure VarRef is the same variable as Var1
    !,
    % Transform to: Var1=(StarlogExpr), (starlog_eval(Var1, Var2), Rest)
    expand_goal_internal(A, EA),
    (Rest = true ->
        Expanded = (EA, starlog:starlog_eval(Var1, Var2))
    ;
        expand_goal_internal(Rest, ER),
        Expanded = (EA, (starlog:starlog_eval(Var1, Var2), ER))
    ).

% Handle conjunctions
expand_goal_internal((A, B), (EA, EB)) :-
    !,
    expand_goal_internal(A, EA),
    expand_goal_internal(B, EB).

% Handle disjunctions
expand_goal_internal((A ; B), (EA ; EB)) :-
    !,
    expand_goal_internal(A, EA),
    expand_goal_internal(B, EB).

% Handle if-then-else
expand_goal_internal((A -> B ; C), (EA -> EB ; EC)) :-
    !,
    expand_goal_internal(A, EA),
    expand_goal_internal(B, EB),
    expand_goal_internal(C, EC).

% Handle if-then
expand_goal_internal((A -> B), (EA -> EB)) :-
    !,
    expand_goal_internal(A, EA),
    expand_goal_internal(B, EB).

% Handle negation
expand_goal_internal(\+ A, \+ EA) :-
    !,
    expand_goal_internal(A, EA).

% Starlog is-expression with both sides being expressions: (Expr1 is Expr2)
% This handles cases like ([1] & A) is (B & [2])
% Special handling for string/atom concatenation dual expressions
expand_goal_internal((LHS is RHS), Expanded) :-
    is_starlog_expr(LHS),
    is_starlog_expr(RHS),
    !,
    % Check if this is a string/atom concat dual expression that needs special handling
    (is_concat_dual_expr(LHS, RHS) ->
        % Use bidirectional constraint solving for string/atom concat
        solve_concat_dual_expr(LHS, RHS, Expanded)
    % Check if this is a list dual expression (without &) with concat on both sides
    ; is_list_dual_expr_with_concat(LHS, RHS) ->
        % Use bidirectional constraint solving for list-to-list with concat elements
        solve_list_dual_expr(LHS, RHS, Expanded)
    % Check if this is a list append dual expression with concat on both sides
    ; is_list_append_dual_expr_with_concat(LHS, RHS) ->
        % Use bidirectional constraint solving for list append with concat elements
        solve_list_append_dual_expr(LHS, RHS, Expanded)
    ;
        % Standard dual expression handling
        compile_starlog_expr(LHS, LHSResult, LHSGoals),
        compile_starlog_expr(RHS, RHSResult, RHSGoals),
        % To prevent infinite backtracking, execute the more ground side first.
        % If RHS is more ground (has fewer variables or is fully ground),
        % execute RHS pre-goals, then both result assignments, then unification,
        % then LHS pre-goals IN REVERSE ORDER. Reversing the LHS pre-goals ensures
        % that when we have a chain like append(A,[x],B), append(B,[y],C), append(C,[z],Result),
        % and Result is already bound, we execute from Result backwards to constrain
        % all intermediate variables before trying to generate solutions.
        (is_more_ground(RHS, LHS) ->
            % Split goals into pre-goals and result goal
            split_result_goal(LHSGoals, LHSPreGoals, LHSResultGoal),
            split_result_goal(RHSGoals, RHSPreGoals, RHSResultGoal),
            % Reverse LHS pre-goals to execute from most constrained to least
            reverse(LHSPreGoals, ReversedLHSPreGoals),
            % Order: RHS pre-goals, RHS result, unification, LHS result, LHS pre-goals (reversed)
            append(RHSPreGoals, [RHSResultGoal, LHSResult = RHSResult, LHSResultGoal | ReversedLHSPreGoals], FinalGoals)
        ;
            append(LHSGoals, [LHSResult = RHSResult|RHSGoals], FinalGoals)
        ),
        list_to_conjunction(FinalGoals, Expanded)
    ).

% Special case: List append dual expression where RHS is a plain value
% Pattern: ([5:(2+2)] & A) is ["54",3] or ([A:4] & [3]) is ["54",3]
% This uses constraint solving when list elements contain concat operations with variables
expand_goal_internal((LHS is RHS), Expanded) :-
    is_starlog_expr(LHS),
    \+ is_starlog_expr(RHS),
    has_append_operator(LHS),
    extract_list_append_parts(LHS, LeftList, RightList),
    !,
    % Choose compilation strategy based on whether list has concat with variables
    (list_has_concat_with_vars(LeftList) ->
        % Use constraint solving for lists with concat expressions and variables
        compile_list_append_constraint(LeftList, RightList, RHS, Expanded)
    ;
        % Standard compilation
        compile_starlog_expr(LHS, LHSResult, LHSGoals),
        % Put the unification before list append operations to prevent infinite backtracking
        % when RHS is ground. This ensures append is called with a bound third argument,
        % which constrains the search space and prevents the generation of infinite solutions.
        append([LHSResult = RHS], LHSGoals, FinalGoals),
        list_to_conjunction(FinalGoals, Expanded)
    ).

% Starlog is-expression with LHS being a Starlog expression: Expr is Out
% This handles cases like reverse(reverse([1,2,3])) is X
% The cut prevents backtracking to less specific clauses below (e.g., the RHS-only clause)
% and ensures this more general LHS pattern is matched after specific LHS patterns above.
expand_goal_internal((LHS is RHS), Expanded) :-
    is_starlog_expr(LHS),
    \+ is_starlog_expr(RHS),
    !,
    compile_starlog_expr(LHS, LHSResult, LHSGoals),
    append(LHSGoals, [LHSResult = RHS], FinalGoals),
    list_to_conjunction(FinalGoals, Expanded).

% Starlog is-expression: Out is Expr
expand_goal_internal((Out is Expr), Expanded) :-
    is_starlog_expr(Expr),
    !,
    compile_starlog_expr(Expr, Out, Goals),
    list_to_conjunction(Goals, Expanded).

% Non-Starlog goal - pass through
expand_goal_internal(Goal, Goal).

% list_to_conjunction(+Goals, -Conjunction)
% Convert a list of goals to a conjunction.
list_to_conjunction([Goal], Goal) :- !.
list_to_conjunction([Goal|Goals], (Goal, Rest)) :-
    list_to_conjunction(Goals, Rest).
list_to_conjunction([], true).

% is_starlog_expr(+Expr)
% Check if an expression is a Starlog expression (not arithmetic).
is_starlog_expr(Expr) :- var(Expr), !, fail.  % Variables are not expressions
is_starlog_expr(_ : _) :- !.
is_starlog_expr(_ & _) :- !.
is_starlog_expr(_ • _) :- !.
is_starlog_expr(..=(_)) :- !.  % List to term conversion
is_starlog_expr(=..(_)) :- !.  % Term to list conversion
is_starlog_expr(no_eval(_)) :- !.  % Special case for no_eval
is_starlog_expr(eval(_)) :- !.  % Special case for eval
is_starlog_expr(Expr) :-
    \+ is_arithmetic(Expr),  % Exclude arithmetic first
    compound(Expr),
    functor(Expr, Name, Arity),
    is_value_builtin(Name, Arity, _),
    !.
is_starlog_expr(Expr) :-
    \+ is_arithmetic(Expr),  % Exclude arithmetic
    contains_starlog_operator(Expr),
    !.

% contains_starlog_operator(+Term)
% Check if a term contains Starlog operators (: & • ..= =..) anywhere in its structure.
contains_starlog_operator(_ : _) :- !.
contains_starlog_operator(_ & _) :- !.
contains_starlog_operator(_ • _) :- !.
contains_starlog_operator(..=(_)) :- !.
contains_starlog_operator(=..(_)) :- !.
contains_starlog_operator(Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    member(Arg, Args),
    contains_starlog_operator(Arg).

% has_append_operator(+Term)
% Check if a term has the & (list append) operator
% This is more specific than contains_starlog_operator as it only checks for &
has_append_operator(_ & _) :- !.
has_append_operator(Expr) :-
    compound(Expr),
    Expr \= (_ : _),  % Exclude other operators for efficiency
    Expr \= (_ • _),
    Expr =.. [_|Args],
    member(Arg, Args),
    has_append_operator(Arg).

% contains_starlog_in_term(+Term)
% Check if a term contains Starlog expressions (is/2 with Starlog operators or special functions)
contains_starlog_in_term(_ is Expr) :-
    is_starlog_expr(Expr),
    !.
contains_starlog_in_term(Term) :-
    compound(Term),
    Term \= (_ is _),  % Not already checked above
    Term =.. [_|Args],
    member(Arg, Args),
    contains_starlog_in_term(Arg).

% is_var_or_starts_with_var(+Goal, +Var)
% Check if Goal is Var or starts with Var (i.e., (Var, Rest))
% Fails if Goal doesn't match either pattern (intentional - used in pattern detection)
is_var_or_starts_with_var(Goal, Var) :-
    Goal == Var,
    !.
is_var_or_starts_with_var((First, _Rest), Var) :-
    First == Var,
    !.

% replace_first_var_with_starlog_call(+Goal, +Var, -NewGoal)
% Replace the first occurrence of Var in Goal with starlog_call(Var)
% If Goal doesn't start with Var, returns Goal unchanged (fallback for non-matching cases)
replace_first_var_with_starlog_call(Goal, Var, starlog:starlog_call(Var)) :-
    Goal == Var,
    !.
replace_first_var_with_starlog_call((First, Rest), Var, (starlog:starlog_call(Var), Rest)) :-
    First == Var,
    !.
% Fallback: if Goal doesn't match expected patterns, return unchanged
% This handles edge cases where the pattern detection succeeded but replacement context differs
replace_first_var_with_starlog_call(Goal, _Var, Goal).

% extract_list_append_parts(+Expr, -LeftList, -RightList)
% Extract the two parts of a list append expression
% Handles: ([A:4] & [3]) -> LeftList = [A:4], RightList = [3]
%          (A & [3]) -> LeftList = A, RightList = [3]
extract_list_append_parts(LeftList & RightList, LeftList, RightList) :- !.

% list_has_concat_with_vars(+List)
% Check if a list contains concatenation expressions with variables
% This includes checking list elements that are concat expressions
list_has_concat_with_vars(List) :-
    is_list(List),
    member(Elem, List),
    (is_concat_expr_with_var(Elem) ; is_concat_expr_with_var_deep(Elem)),
    !.

% is_concat_expr_with_var(+Expr)
% Check if expression is a concat with at least one variable
is_concat_expr_with_var(Expr) :- 
    is_concat_with_check(Expr, has_var).

% is_concat_expr_with_var_deep(+Expr)
% Check recursively for concat expressions with variables
is_concat_expr_with_var_deep(A : _) :- var(A), !.
is_concat_expr_with_var_deep(_ : B) :- var(B), !.
is_concat_expr_with_var_deep(A • _) :- var(A), !.
is_concat_expr_with_var_deep(_ • B) :- var(B), !.
is_concat_expr_with_var_deep(Expr) :- 
    is_concat_with_check(Expr, deep_check).

% is_concat_with_check(+Expr, +CheckType)
% Helper to check concat expressions with different check types
is_concat_with_check(A : B, has_var) :- !, (var(A) ; var(B)).
is_concat_with_check(A • B, has_var) :- !, (var(A) ; var(B)).
is_concat_with_check(A : B, deep_check) :- 
    !,
    (is_concat_expr_with_var_deep(A) ; is_concat_expr_with_var_deep(B)).
is_concat_with_check(A • B, deep_check) :- 
    !,
    (is_concat_expr_with_var_deep(A) ; is_concat_expr_with_var_deep(B)).

% compile_list_append_constraint(+LeftList, +RightList, +RHS, -Goals)
% Compile a list append expression with constraint solving for concat operations
% Pattern: ([A:4] & [3]) is ["54",3]
% Strategy: Create variables for concat results, do append, then add constraints
compile_list_append_constraint(LeftList, RightList, RHS, Goals) :-
    (is_list(LeftList) ->
        % LeftList is a concrete list with potential concat expressions
        % Create result variables for each concat expression
        create_list_with_concat_vars(LeftList, ProcessedLeft, Constraints),
        compile_value(RightList, RightVal, RightGoals),
        % First do the append to unify the structure
        append(RightGoals, [append(ProcessedLeft, RightVal, RHS)], PreGoals),
        % Then add the concat constraints
        append(PreGoals, Constraints, AllGoals),
        list_to_conjunction(AllGoals, Goals)
    ;
        % LeftList is a variable or expression, use standard append
        compile_value(LeftList, LeftVal, LeftGoals),
        compile_value(RightList, RightVal, RightGoals),
        append(LeftGoals, RightGoals, PreGoals),
        append(PreGoals, [append(LeftVal, RightVal, RHS)], AllGoals),
        list_to_conjunction(AllGoals, Goals)
    ).

% create_list_with_concat_vars(+InputList, -OutputList, -Constraints)
% For each concat expression, create a fresh variable and a constraint
% Example: [A:4, B] -> [_G1, B], [string_concat(A, 4, _G1)]
create_list_with_concat_vars([], [], []).
create_list_with_concat_vars([Elem|Rest], [ResultVar|ProcessedRest], AllConstraints) :-
    (is_concat_operation(Elem) ->
        % Element is a concat expression - create constraint
        create_concat_constraint(Elem, ResultVar, ElemConstraints),
        create_list_with_concat_vars(Rest, ProcessedRest, RestConstraints),
        append(ElemConstraints, RestConstraints, AllConstraints)
    ;
        % Element is not a concat expression, process it normally
        (is_starlog_expr(Elem) ->
            compile_starlog_expr(Elem, ResultVar, ElemGoals),
            create_list_with_concat_vars(Rest, ProcessedRest, RestConstraints),
            append(ElemGoals, RestConstraints, AllConstraints)
        ;
            % Plain value
            ResultVar = Elem,
            create_list_with_concat_vars(Rest, ProcessedRest, AllConstraints)
        )
    ).

% is_concat_operation(+Expr)
% Check if expression is a concatenation operation
is_concat_operation(_ : _) :- !.
is_concat_operation(_ • _) :- !.

% create_concat_constraint(+ConcatExpr, -ResultVar, -Constraints)
% Create a constraint for a concat expression
% The ResultVar will be unified by append, and the constraint will solve the concat
create_concat_constraint((A : B), ResultVar, Constraints) :-
    !,
    compile_value(A, AVal, AGoals),
    compile_value(B, BVal, BGoals),
    append(AGoals, BGoals, PreGoals),
    append(PreGoals, [string_concat(AVal, BVal, ResultVar)], Constraints).
create_concat_constraint((A • B), ResultVar, Constraints) :-
    !,
    compile_value(A, AVal, AGoals),
    compile_value(B, BVal, BGoals),
    append(AGoals, BGoals, PreGoals),
    append(PreGoals, [atom_concat(AVal, BVal, ResultVar)], Constraints).

% is_arithmetic(+Expr)
% Check if expression is arithmetic (should use Prolog is/2).
is_arithmetic(Expr) :-
    compound(Expr),
    functor(Expr, Op, 2),
    memberchk(Op, [+, -, *, /, //, mod, **, ^]).
is_arithmetic(Expr) :-
    compound(Expr),
    functor(Expr, Op, 1),
    memberchk(Op, [-, abs, sign, min, max]).
is_arithmetic(X) :- number(X).

% is_concat_dual_expr(+LHS, +RHS)
% Check if both sides are string or atom concatenation expressions
is_concat_dual_expr((_ : _), (_ : _)) :- !.
is_concat_dual_expr((_ • _), (_ • _)) :- !.

% is_list_dual_expr_with_concat(+LHS, +RHS)
% Check if both sides are lists (not with &) with concat operations
% Pattern: [A:a] is [b:B] or [(A•a):c] is [(b•B):c]
is_list_dual_expr_with_concat(LHS, RHS) :-
    % Both sides must be lists
    is_list(LHS),
    is_list(RHS),
    % Both lists must have concat operations
    list_has_concat_operations(LHS),
    list_has_concat_operations(RHS).

% is_list_append_dual_expr_with_concat(+LHS, +RHS)
% Check if both sides are list append expressions with concat operations in list elements
% Pattern: ([A•b] & [d]) is [a•B, d] or similar
is_list_append_dual_expr_with_concat(LHS, RHS) :-
    % LHS has append operator
    has_append_operator(LHS),
    % Extract parts from LHS
    extract_list_append_parts(LHS, LHSLeft, LHSRight),
    % LHS has concat operations in lists
    (list_has_concat_with_vars(LHSLeft) ; list_has_concat_with_vars(LHSRight)),
    % RHS is a list (could be plain or have concat operations)
    (is_list(RHS) ; has_append_operator(RHS)),
    % Check if RHS has concat operations
    (is_list(RHS) ->
        list_has_concat_operations(RHS)
    ;
        % RHS has append operator, extract and check
        extract_list_append_parts(RHS, RHSLeft, RHSRight),
        (list_has_concat_operations(RHSLeft) ; list_has_concat_operations(RHSRight))
    ).

% list_has_concat_operations(+List)
% Check if a list contains concatenation operations (not just variables)
% This recursively checks nested lists to support patterns like [[A:a]] is [[a:a]]
list_has_concat_operations(List) :-
    is_list(List),
    member(Elem, List),
    (is_concat_operation(Elem) ; (is_list(Elem), list_has_concat_operations(Elem))),
    !.

% solve_list_append_dual_expr(+LHS, +RHS, -Goals)
% Solve dual list append expressions where both sides have concat operations
% Pattern: ([A•b] & [d]) is [a•B, d]
% Strategy: 
% 1. Extract list parts from both sides
% 2. Create variables for concat results on both sides
% 3. Set up append constraint
% 4. Add bidirectional concat constraints
solve_list_append_dual_expr(LHS, RHS, Goals) :-
    % Extract parts from LHS
    extract_list_append_parts(LHS, LHSLeft, LHSRight),
    % Process RHS - could be a plain list or have append operator
    (is_list(RHS) ->
        % RHS is a plain list - this is the common case
        RHSList = RHS,
        RHSAppendGoals = []
    ;
        % RHS has append operator
        extract_list_append_parts(RHS, RHSLeft, RHSRight),
        % Compile the RHS append
        compile_value(RHSRight, RHSRightVal, RHSRightGoals),
        RHSAppendGoals = RHSRightGoals,
        % We'll need to append the parts together
        % Use a compound term to clearly indicate this is a placeholder
        RHSList = placeholder(rhs_will_be_computed)
    ),
    % Create processed lists with variables for concat operations
    create_list_with_dual_concat_vars(LHSLeft, ProcessedLHSLeft, LHSLeftConstraints, '_L'),
    % Also process LHSRight if it's a list (it might have concat operations too)
    (is_list(LHSRight) ->
        create_list_with_dual_concat_vars(LHSRight, ProcessedLHSRight, LHSRightConstraints, '_L'),
        LHSRightGoals = [],
        LHSRightVal = ProcessedLHSRight
    ;
        compile_value(LHSRight, LHSRightVal, LHSRightGoals),
        LHSRightConstraints = []
    ),
    % Combine all LHS constraints
    append(LHSLeftConstraints, LHSRightConstraints, AllLHSConstraints),
    % For RHS, we need to process it based on its structure
    (RHSList = placeholder(rhs_will_be_computed) ->
        % RHS has append - process both parts
        create_list_with_dual_concat_vars(RHSLeft, ProcessedRHSLeft, RHSLeftConstraints, '_R'),
        (is_list(RHSRight) ->
            create_list_with_dual_concat_vars(RHSRight, ProcessedRHSRightList, RHSRightListConstraints, '_R'),
            RHSRightGoals2 = [],
            RHSRightVal = ProcessedRHSRightList,
            AllRHSRightConstraints = RHSRightListConstraints
        ;
            compile_value(RHSRight, RHSRightVal, RHSRightGoals2),
            AllRHSRightConstraints = []
        ),
        append(RHSAppendGoals, RHSRightGoals2, RHSGoals1),
        % Build the RHS by appending
        append(RHSGoals1, [append(ProcessedRHSLeft, RHSRightVal, RHSResult)], RHSGoals),
        append(RHSLeftConstraints, AllRHSRightConstraints, AllRHSConstraints)
    ;
        % RHS is a plain list
        create_list_with_dual_concat_vars(RHSList, ProcessedRHSList, RHSConstraints, '_R'),
        RHSResult = ProcessedRHSList,
        RHSGoals = [],
        AllRHSConstraints = RHSConstraints
    ),
    % Build LHS by appending
    append(LHSRightGoals, [append(ProcessedLHSLeft, LHSRightVal, LHSResult)], LHSGoals),
    % Unify the results
    append(LHSGoals, RHSGoals, PreGoals),
    append(PreGoals, [LHSResult = RHSResult], UnifyGoals),
    % Now add the bidirectional concat constraints
    % Pair up constraints from LHS and RHS
    create_bidirectional_constraints(AllLHSConstraints, AllRHSConstraints, BidiConstraints),
    % Combine all goals
    append(UnifyGoals, BidiConstraints, AllGoals),
    list_to_conjunction(AllGoals, Goals).

% solve_list_dual_expr(+LHS, +RHS, -Goals)
% Solve list dual expressions (without &) where both sides have concat operations
% Pattern: [A:a] is [b:B] or [(A•a):c] is [(b•B):c]
% Strategy:
% 1. Process both lists to extract concat constraints
% 2. Create result variables for concat operations
% 3. Pair up constraints from LHS and RHS
% 4. Add bidirectional concat constraints
% 5. Unify the processed lists
solve_list_dual_expr(LHS, RHS, Goals) :-
    % Both are lists - process them
    create_list_with_dual_concat_vars(LHS, ProcessedLHS, LHSConstraints, '_L'),
    create_list_with_dual_concat_vars(RHS, ProcessedRHS, RHSConstraints, '_R'),
    % Pair up the concat constraints
    create_bidirectional_constraints(LHSConstraints, RHSConstraints, BidiConstraints),
    % Combine goals: bidirectional constraints first, then unification
    % This ensures concat operations are executed before we try to unify results
    append(BidiConstraints, [ProcessedLHS = ProcessedRHS], AllGoals),
    list_to_conjunction(AllGoals, Goals).

% create_list_with_dual_concat_vars(+InputList, -OutputList, -Constraints, +Prefix)
% Similar to create_list_with_concat_vars but returns constraints as a list for pairing
% Prefix helps distinguish variables from LHS and RHS
create_list_with_dual_concat_vars([], [], [], _).
create_list_with_dual_concat_vars([Elem|Rest], [ResultVar|ProcessedRest], AllConstraints, Prefix) :-
    (is_concat_operation(Elem) ->
        % Element is a concat expression - create constraint info
        create_dual_concat_constraint_info(Elem, ResultVar, ElemConstraint, Prefix),
        create_list_with_dual_concat_vars(Rest, ProcessedRest, RestConstraints, Prefix),
        AllConstraints = [ElemConstraint|RestConstraints]
    ;
        % Element is not a concat expression
        ResultVar = Elem,
        create_list_with_dual_concat_vars(Rest, ProcessedRest, AllConstraints, Prefix)
    ).

% create_dual_concat_constraint_info(+ConcatExpr, -ResultVar, -ConstraintInfo, +Prefix)
% Create constraint information for a concat expression (not yet executable goals)
% ConstraintInfo is concat_constraint(Op, A, B, ResultVar) where Op is ':' or '•'
% Prefix parameter is reserved for future use to distinguish LHS/RHS variables if needed
create_dual_concat_constraint_info((A : B), ResultVar, concat_constraint(':', A, B, ResultVar), _Prefix) :- !.
create_dual_concat_constraint_info((A • B), ResultVar, concat_constraint('•', A, B, ResultVar), _Prefix) :- !.

% create_bidirectional_constraints(+LHSConstraints, +RHSConstraints, -Goals)
% Pair up constraints from LHS and RHS and create bidirectional solving goals
% If lists have same length, pair them up and solve bidirectionally
% Pattern: [concat_constraint('•', A, b, R1)] and [concat_constraint('•', a, B, R2)]
%   should create: atom_concat(A, b, R1), atom_concat(a, B, R2), R1 = R2
% NOTE: If constraint lists don't match in length, this may indicate an error in pattern matching
create_bidirectional_constraints([], [], []) :- !.
create_bidirectional_constraints([LHSConstr|LHSRest], [RHSConstr|RHSRest], Goals) :-
    !,
    % Process this pair
    create_constraint_pair_goals(LHSConstr, RHSConstr, PairGoals),
    % Process rest
    create_bidirectional_constraints(LHSRest, RHSRest, RestGoals),
    % Combine
    append(PairGoals, RestGoals, Goals).
% Fallback if lists don't match - should rarely happen
% This could indicate a pattern mismatch that we're gracefully handling
create_bidirectional_constraints(_, _, []).

% create_constraint_pair_goals(+LHSConstr, +RHSConstr, -Goals)
% Create executable goals for a pair of concat constraints
% This uses bidirectional constraint solving
create_constraint_pair_goals(
    concat_constraint(':', LHS_A, LHS_B, LHS_R),
    concat_constraint(':', RHS_A, RHS_B, RHS_R),
    [starlog_expand:string_concat_dual(LHS_A, LHS_B, RHS_A, RHS_B), LHS_R = RHS_R]
) :- !.
create_constraint_pair_goals(
    concat_constraint('•', LHS_A, LHS_B, LHS_R),
    concat_constraint('•', RHS_A, RHS_B, RHS_R),
    [starlog_expand:atom_concat_dual(LHS_A, LHS_B, RHS_A, RHS_B), LHS_R = RHS_R]
) :- !.
% If operations don't match or other cases, just execute each independently
create_constraint_pair_goals(LHSConstr, RHSConstr, Goals) :-
    constraint_to_goals(LHSConstr, LHSGoals),
    constraint_to_goals(RHSConstr, RHSGoals),
    append(LHSGoals, RHSGoals, Goals).

% constraint_to_goals(+Constraint, -Goals)
% Convert a constraint info to executable goals
constraint_to_goals(concat_constraint(':', A, B, R), [string_concat(A, B, R)]) :- !.
constraint_to_goals(concat_constraint('•', A, B, R), [atom_concat(A, B, R)]) :- !.

% solve_concat_dual_expr(+LHS, +RHS, -Goals)
% Solve dual expressions involving string or atom concatenation
% Pattern: (A:B) is (C:D) means string_concat(A,B) = string_concat(C,D)
% Pattern: (A•B) is (C•D) means atom_concat(A,B) = atom_concat(C,D)
% Also handles nested patterns like (a:A:c) is (B:b:c)

% String concatenation dual expression
solve_concat_dual_expr((A : B), (C : D), Goals) :-
    !,
    % Use bidirectional string concat constraint solver
    Goals = starlog_expand:string_concat_dual(A, B, C, D).

% Atom concatenation dual expression  
solve_concat_dual_expr((A • B), (C • D), Goals) :-
    !,
    % Use bidirectional atom concat constraint solver
    Goals = starlog_expand:atom_concat_dual(A, B, C, D).

% concat_dual(?A, ?B, ?C, ?D, +ConcatPred)
% Generic bidirectional constraint solver for concat operations
% ConcatPred should be string_concat or atom_concat
% Handles both simple binary and nested concatenation expressions
concat_dual(A, B, C, D, ConcatPred) :-
    % Check if B and D are equal (same suffix) - this handles patterns like (a:A:c) is (B:b:c)
    % where both sides end with 'c'
    (B == D, ground(B)) ->
        % If B = D and they're bound, then A+B = C+D simplifies to A+B = C+B, so A = C
        (is_concat_expr(A, ConcatPred),
         is_concat_expr(C, ConcatPred) ->
            % Both A and C are concat expressions - solve them recursively
            solve_nested_concat_dual(A, C, ConcatPred)
        ;
            % Simple case: A = C
            A = C
        )
    ;
    % Check if A and C are equal (same prefix) - this handles patterns like (a:X:Y) is (a:Z:W)
    (A == C, ground(A)) ->
        % If A = C and they're bound, then A+B = C+D simplifies to A+B = A+D, so B = D
        (is_concat_expr(B, ConcatPred),
         is_concat_expr(D, ConcatPred) ->
            % Both B and D are concat expressions - solve them recursively
            solve_nested_concat_dual(B, D, ConcatPred)
        ;
            % Simple case: B = D
            B = D
        )
    ;
    % New case: Both B and D are concat expressions (not necessarily identical)
    % Pattern: (A:a:C) is (b:a:c) which becomes (A:(a:C)) is (b:(a:c))
    % This handles nested concatenation on the right side
    (is_concat_expr(B, ConcatPred), is_concat_expr(D, ConcatPred)) ->
        % Recursively solve B is D, which constrains variables in B and D
        solve_nested_concat_dual(B, D, ConcatPred),
        % Then solve A and C: A+B = C+D, and we know B and D from above
        % Since B and D are now constrained to be equal (from the recursive solve),
        % we have A+B = C+B, so A = C
        A = C
    ;
    % New case: Both A and C are concat expressions (not necessarily identical)
    % Pattern: (a:A:b) is (a:C:b) which doesn't simplify by prefix/suffix
    % This handles nested concatenation on the left side
    (is_concat_expr(A, ConcatPred), is_concat_expr(C, ConcatPred)) ->
        % Recursively solve A is C, which constrains variables in A and C
        solve_nested_concat_dual(A, C, ConcatPred),
        % Then B = D since A+B = C+D and A = C
        B = D
    ;
    % If all are bound, just check equality
    (ground(A), ground(B), ground(C), ground(D)) ->
        (call(ConcatPred, A, B, R1),
         call(ConcatPred, C, D, R2),
         R1 = R2)
    ;
    % Special case: A and B are bound, C and D are variables
    % Constraint: A + B = C + D. Solution: C = A and D = B
    % This handles patterns like (c•d) is (C•D) where we want C=c, D=d
    % Rationale: If we know the parts on the left (A and B), the bidirectional
    % solution is to use the same parts on the right (C=A, D=B) so that
    % A+B equals C+D (since C+D becomes A+B)
    (nonvar(A), nonvar(B), var(C), var(D)) ->
        (C = A,
         D = B)
    ;
    % Special case: A and D are bound, B and C are variables
    % Constraint: A + B = C + D. Solution: B = D and C = A
    % This gives: A + B = A + D (LHS) and C + D = A + D (RHS), so both sides equal
    (nonvar(A), var(B), var(C), nonvar(D)) ->
        (B = D,
         C = A)
    ;
    % Special case: B and C are bound, A and D are variables  
    % Constraint: A + B = C + D. Solution: A = C and D = B
    % This gives: A + B = C + B (LHS) and C + D = C + B (RHS), so both sides equal
    (var(A), nonvar(B), nonvar(C), var(D)) ->
        (A = C,
         D = B)
    ;
    % Special case: Only A is variable, B, C, D are all bound
    % Constraint: A + B = C + D. We need to solve for A.
    % Strategy: Compute result on RHS, then extract A from LHS
    (var(A), nonvar(B), nonvar(C), nonvar(D)) ->
        (call(ConcatPred, C, D, Result),
         % Now solve A + B = Result for A
         % Use the same predicate for reverse mode
         call(ConcatPred, A, B, Result))
    ;
    % Fallback: use standard constraint with both concat calls
    % This handles cases where both sides need to be evaluated and unified
    % Works when there are enough bound values to constrain the solution
        (call(ConcatPred, A, B, Result),
         call(ConcatPred, C, D, Result)).

% string_concat_dual(?A, ?B, ?C, ?D)
% Bidirectional constraint solver for (A:B) is (C:D)
% Meaning: string_concat(A, B, Result) and string_concat(C, D, Result)
% So: A + B = C + D where + is string concatenation
string_concat_dual(A, B, C, D) :-
    concat_dual(A, B, C, D, string_concat).

% atom_concat_dual(?A, ?B, ?C, ?D)
% Bidirectional constraint solver for (A•B) is (C•D)
% Meaning: atom_concat(A, B, Result) and atom_concat(C, D, Result)
% So: A + B = C + D where + is atom concatenation
atom_concat_dual(A, B, C, D) :-
    concat_dual(A, B, C, D, atom_concat).

% is_concat_expr(+Expr, +ConcatPred)
% Check if Expr is a concatenation expression for the given predicate
is_concat_expr((_ : _), string_concat) :- !.
is_concat_expr((_ • _), atom_concat) :- !.

% solve_nested_concat_dual(+LHS, +RHS, +ConcatPred)
% Solve nested concatenation dual expressions
% For string_concat: solve (A:B) is (C:D) where A,B,C,D may be concat exprs
% For atom_concat: solve (A•B) is (C•D) where A,B,C,D may be concat exprs
solve_nested_concat_dual((A : B), (C : D), string_concat) :-
    !,
    concat_dual(A, B, C, D, string_concat).
solve_nested_concat_dual((A • B), (C • D), atom_concat) :-
    !,
    concat_dual(A, B, C, D, atom_concat).

% compile_starlog_expr(+Expr, +Out, -Goals)
% Compile a Starlog expression into Prolog goal(s).

% String concatenation: Out is (A : B)
compile_starlog_expr((A : B), Out, Goals) :-
    !,
    compile_value(A, AVal, AGoals),
    compile_value(B, BVal, BGoals),
    append(AGoals, BGoals, PreGoals),
    append(PreGoals, [string_concat(AVal, BVal, Out)], Goals).

% List append: Out is (A & B)
compile_starlog_expr((A & B), Out, Goals) :-
    !,
    compile_value(A, AVal, AGoals),
    compile_value(B, BVal, BGoals),
    append(AGoals, BGoals, PreGoals),
    append(PreGoals, [append(AVal, BVal, Out)], Goals).

% Atom concatenation: Out is (A • B)
compile_starlog_expr((A • B), Out, Goals) :-
    !,
    compile_value(A, AVal, AGoals),
    compile_value(B, BVal, BGoals),
    append(AGoals, BGoals, PreGoals),
    append(PreGoals, [atom_concat(AVal, BVal, Out)], Goals).

% List to term conversion: Out is ..=(List)
% Converts a list to a term using the univ operator
% Example: A is ..=([f,0,1]) gives A = f(0,1)
compile_starlog_expr(..=(List), Out, Goals) :-
    !,
    compile_value(List, ListVal, ListGoals),
    append(ListGoals, [Out =.. ListVal], Goals).

% Term to list conversion: Out is =..(Term)
% Converts a term to a list using the univ operator
% Example: A is =..(f(0,1)) gives A = [f,0,1]
compile_starlog_expr(=..(Term), Out, Goals) :-
    !,
    compile_value(Term, TermVal, TermGoals),
    append(TermGoals, [TermVal =.. Out], Goals).

% Special case: no_eval(Expr) - returns expression without evaluating
% But first check if Expr contains eval() expressions that need to be processed
compile_starlog_expr(no_eval(Expr), Out, Goals) :-
    !,
    (contains_eval(Expr) ->
        % Process eval() expressions inside no_eval
        process_eval_in_no_eval(Expr, ProcessedExpr, Goals1),
        append(Goals1, [Out = ProcessedExpr], Goals)
    ;
        % No eval() inside, just return the expression as-is
        Goals = [Out = Expr]
    ).

% Special case: eval(Expr) - forces evaluation of expression
compile_starlog_expr(eval(Expr), Out, Goals) :-
    !,
    compile_starlog_expr(Expr, Out, Goals).

% Value-returning builtin: Out is func(Args...)
compile_starlog_expr(Expr, Out, Goals) :-
    compound(Expr),
    Expr =.. [Name|Args],
    is_value_builtin(Name, Arity, PrologPred),
    length(Args, Arity),
    !,
    compile_values(Args, Vals, PreGoals),
    append(Vals, [Out], PrologArgs),
    PrologGoal =.. [PrologPred|PrologArgs],
    append(PreGoals, [PrologGoal], Goals).

% Nullary builtin: Out is func
compile_starlog_expr(Expr, Out, [PrologGoal]) :-
    atom(Expr),
    is_value_builtin(Expr, 0, PrologPred),
    !,
    PrologGoal =.. [PrologPred, Out].

% Arithmetic expression - keep as is/2
compile_starlog_expr(Expr, Out, [Out is Expr]) :-
    is_arithmetic(Expr),
    !.

% Compound term with Starlog operators in arguments (not a registered builtin)
compile_starlog_expr(Expr, Out, Goals) :-
    contains_starlog_operator(Expr),
    !,
    Expr =.. [Functor|Args],
    compile_values(Args, Vals, PreGoals),
    Result =.. [Functor|Vals],
    append(PreGoals, [Out = Result], Goals).

% Unknown - treat as value
compile_starlog_expr(Expr, Out, [Out = Expr]).

% compile_value(+Expr, -Value, -Goals)
% Compile a value expression into goals and a result value.
compile_value(Expr, Value, Goals) :-
    is_starlog_expr(Expr),
    !,
    compile_starlog_expr(Expr, Value, Goals).
% Arithmetic expressions should be evaluated when used as builtin arguments
compile_value(Expr, Value, [Value is Expr]) :-
    is_arithmetic(Expr),
    !.
compile_value(Expr, Expr, []).

% compile_values(+Exprs, -Values, -Goals)
% Compile a list of value expressions.
compile_values([], [], []).
compile_values([E|Es], [V|Vs], Goals) :-
    compile_value(E, V, EGoals),
    compile_values(Es, Vs, EsGoals),
    append(EGoals, EsGoals, Goals).

% contains_eval(+Expr)
% Check if an expression contains eval() anywhere in its structure
% Uses a direct recursive approach without generating choice points
contains_eval(eval(_)) :- !.
contains_eval(Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    contains_eval_in_list(Args).

% contains_eval_in_list(+Args)
% Check if any argument in the list contains eval()
contains_eval_in_list([Arg|_]) :-
    contains_eval(Arg), !.
contains_eval_in_list([_|Rest]) :-
    contains_eval_in_list(Rest).

% process_eval_in_no_eval(+Expr, -ProcessedExpr, -Goals)
% Process eval() expressions inside a no_eval context
% This evaluates eval() subexpressions while preserving the rest
process_eval_in_no_eval(eval(Inner), Value, Goals) :-
    !,
    % Found an eval - compile it normally to evaluate it
    compile_starlog_expr(Inner, Value, Goals).
process_eval_in_no_eval(Expr, ProcessedExpr, Goals) :-
    compound(Expr),
    !,
    Expr =.. [Functor|Args],
    process_eval_in_args(Args, ProcessedArgs, Goals),
    ProcessedExpr =.. [Functor|ProcessedArgs].
% Atomic values and non-compound terms that don't contain eval - return as-is
process_eval_in_no_eval(Expr, Expr, []).

% process_eval_in_args(+Args, -ProcessedArgs, -Goals)
% Process a list of arguments, evaluating any eval() expressions
% Combines detection and processing in a single pass for efficiency
process_eval_in_args([], [], []).
process_eval_in_args([Arg|Args], [ProcessedArg|ProcessedArgs], Goals) :-
    process_eval_in_no_eval(Arg, ProcessedArg, ArgGoals),
    process_eval_in_args(Args, ProcessedArgs, RestGoals),
    append(ArgGoals, RestGoals, Goals).

% is_more_ground(+Expr1, +Expr2)
% Check if Expr1 is more ground than Expr2.
% An expression is more ground if it has fewer unbound variables.
% This is used to determine execution order in dual expressions to prevent
% infinite backtracking.
is_more_ground(Expr1, Expr2) :-
    % Count variables in each expression
    term_variables(Expr1, Vars1),
    term_variables(Expr2, Vars2),
    length(Vars1, Count1),
    length(Vars2, Count2),
    % Expr1 is more ground if it has fewer variables
    Count1 < Count2.

% split_result_goal(+Goals, -PreGoals, -ResultGoal)
% Split a list of goals into pre-goals and the final result goal.
% The result goal is typically a unification like Out = Result.
% If Goals is empty or has only one goal, PreGoals is empty and ResultGoal is the single goal.
split_result_goal([], [], true) :- !.
split_result_goal([Goal], [], Goal) :- !.
split_result_goal(Goals, PreGoals, ResultGoal) :-
    append(PreGoals, [ResultGoal], Goals),
    !.
