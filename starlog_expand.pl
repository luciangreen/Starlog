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
    ;
        % Standard dual expression handling
        compile_starlog_expr(LHS, LHSResult, LHSGoals),
        compile_starlog_expr(RHS, RHSResult, RHSGoals),
        append(LHSGoals, [LHSResult = RHSResult|RHSGoals], FinalGoals),
        list_to_conjunction(FinalGoals, Expanded)
    ).

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
is_starlog_expr(_) :- fail.

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
    (B == D, nonvar(B)) ->
        % If B = D, then A+B = C+D simplifies to A+B = C+B, so A = C
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
    (A == C, nonvar(A)) ->
        % If A = C, then A+B = C+D simplifies to A+B = A+D, so B = D
        (is_concat_expr(B, ConcatPred),
         is_concat_expr(D, ConcatPred) ->
            % Both B and D are concat expressions - solve them recursively
            solve_nested_concat_dual(B, D, ConcatPred)
        ;
            % Simple case: B = D
            B = D
        )
    ;
    % If all are bound, just check equality
    (ground(A), ground(B), ground(C), ground(D)) ->
        (call(ConcatPred, A, B, R1),
         call(ConcatPred, C, D, R2),
         R1 = R2)
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
is_concat_expr(_, _) :- fail.

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
