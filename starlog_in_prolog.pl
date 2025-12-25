% starlog_in_prolog.pl
% Main library for Starlog-in-Prolog
% Allows developers to write Starlog syntax directly in Prolog files
% and have it expanded automatically at load-time.

:- module(starlog_in_prolog, [
    starlog_call/1,
    starlog_register_value_builtin/3,
    starlog_unregister_value_builtin/2,
    starlog_set_debug/1,
    starlog_show_expansion/1
]).

:- use_module(starlog_expand).
:- use_module(starlog_registry).

% Define helper predicates in user module
:- multifile user:string_number/2.
:- dynamic user:string_number/2.

% string_number(?String, ?Number)
% Convert between string and number (wrapper for number_string with swapped args)
user:string_number(String, Number) :-
    number_string(Number, String).

% Define Starlog operators globally (in user module)
:- op(700, xfx, user:(is)).
:- op(600, yfx, user:(':' )).
:- op(600, yfx, user:('&')).
:- op(600, yfx, user:('•')).

% Install goal expansion hook
:- multifile user:goal_expansion/2.
:- dynamic user:goal_expansion/2.

user:goal_expansion(Goal, Expanded) :-
    \+ current_prolog_flag(xref, true),  % Don't expand during cross-referencing
    starlog_expand:expand_starlog_goal(Goal, Expanded),
    Goal \== Expanded,  % Only expand if something changed
    !.

% Install term expansion hook for whole clauses
:- multifile user:term_expansion/2.
:- dynamic user:term_expansion/2.

user:term_expansion(Term, Expanded) :-
    \+ current_prolog_flag(xref, true),  % Don't expand during cross-referencing
    starlog_expand:expand_starlog_term(Term, Expanded),
    Term \== Expanded.

% goals_to_conjunction(+Goals, -Conjunction)
% Convert a list of goals to a conjunction.
goals_to_conjunction([Goal], Goal) :- !.
goals_to_conjunction([Goal|Goals], (Goal, Rest)) :-
    goals_to_conjunction(Goals, Rest).
goals_to_conjunction([], true).

% starlog_call(+Goal)
% Execute a Starlog goal by expanding it first.
% This is useful for interactive queries in the REPL.
starlog_call(Goal) :-
    starlog_expand:expand_starlog_goal(Goal, ExpandedGoal),
    call(ExpandedGoal).

% starlog_register_value_builtin(+Name, +Arity, +PrologPred)
% Register a new value-returning builtin.
% Example: starlog_register_value_builtin(foo, 2, foo)
%   means: Out is foo(A,B) expands to foo(A,B,Out)
starlog_register_value_builtin(Name, Arity, PrologPred) :-
    starlog_registry:register_value_builtin(Name, Arity, PrologPred).

% starlog_unregister_value_builtin(+Name, +Arity)
% Remove a custom value builtin mapping.
starlog_unregister_value_builtin(Name, Arity) :-
    starlog_registry:unregister_value_builtin(Name, Arity).

% starlog_set_debug(+Flag)
% Enable/disable debug output (true/false).
starlog_set_debug(Flag) :-
    starlog_expand:set_starlog_debug(Flag).

% starlog_show_expansion(+Goal)
% Show the Starlog expansion of a goal with intermediate variables.
% This converts the expanded Prolog goals back to Starlog notation.
starlog_show_expansion(Goal) :-
    starlog_expand:expand_starlog_goal(Goal, ExpandedGoal),
    format('Original Starlog:~n  ~w~n~n', [Goal]),
    format('Expanded to Prolog:~n  ~w~n~n', [ExpandedGoal]),
    prolog_to_starlog_steps(ExpandedGoal, StarlogSteps),
    format('Step-by-step Starlog:~n'),
    print_starlog_steps(StarlogSteps, 1).

% prolog_to_starlog_steps(+PrologGoal, -StarlogSteps)
% Convert expanded Prolog goals back to Starlog notation with intermediate variables.
prolog_to_starlog_steps((Goal, Rest), [Step|Steps]) :-
    !,
    prolog_goal_to_starlog(Goal, Step),
    prolog_to_starlog_steps(Rest, Steps).
prolog_to_starlog_steps(Goal, [Step]) :-
    prolog_goal_to_starlog(Goal, Step).

% prolog_goal_to_starlog(+PrologGoal, -StarlogForm)
% Convert a single Prolog goal to Starlog notation.
prolog_goal_to_starlog(string_concat(A, B, C), starlog(C, (A:B))) :- !.
prolog_goal_to_starlog(append(A, B, C), starlog(C, (A&B))) :- !.
prolog_goal_to_starlog(atom_concat(A, B, C), starlog(C, (A•B))) :- !.
prolog_goal_to_starlog((Out = Expr), assign(Out, Expr)) :- !.
prolog_goal_to_starlog(Goal, StarlogForm) :-
    Goal =.. [Pred|Args],
    Args \= [],  % Ensure there are arguments
    append(InArgs, [OutArg], Args),
    InArgs \= [],  % Ensure there are input arguments
    !,
    InTerm =.. [Pred|InArgs],
    StarlogForm = starlog(OutArg, InTerm).
prolog_goal_to_starlog(Goal, prolog(Goal)).

% print_starlog_steps(+Steps, +N)
% Print the Starlog steps with numbering.
print_starlog_steps([], _).
print_starlog_steps([starlog(Out, Expr)|Rest], N) :-
    !,
    format('  ~w. ~w is ~w~n', [N, Out, Expr]),
    N1 is N + 1,
    print_starlog_steps(Rest, N1).
print_starlog_steps([assign(Out, Expr)|Rest], N) :-
    !,
    format('  ~w. ~w = ~w~n', [N, Out, Expr]),
    N1 is N + 1,
    print_starlog_steps(Rest, N1).
print_starlog_steps([prolog(Goal)|Rest], N) :-
    format('  ~w. ~w~n', [N, Goal]),
    N1 is N + 1,
    print_starlog_steps(Rest, N1).
