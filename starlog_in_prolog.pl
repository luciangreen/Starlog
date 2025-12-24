% starlog_in_prolog.pl
% Main library for Starlog-in-Prolog
% Allows developers to write Starlog syntax directly in Prolog files
% and have it expanded automatically at load-time.

:- module(starlog_in_prolog, [
    starlog_call/1,
    starlog_register_value_builtin/3,
    starlog_unregister_value_builtin/2,
    starlog_set_debug/1
]).

:- use_module(starlog_expand).
:- use_module(starlog_registry).

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
