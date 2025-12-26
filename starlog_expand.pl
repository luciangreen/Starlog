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
replace_first_var_with_starlog_call(Goal, Var, starlog_in_prolog:starlog_call(Var)) :-
    Goal == Var,
    !.
replace_first_var_with_starlog_call((First, Rest), Var, (starlog_in_prolog:starlog_call(Var), Rest)) :-
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
