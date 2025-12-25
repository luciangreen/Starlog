% starlog_in_prolog.pl
% Main library for Starlog-in-Prolog
% Allows developers to write Starlog syntax directly in Prolog files
% and have it expanded automatically at load-time.

:- module(starlog_in_prolog, [
    starlog_call/1,
    starlog_register_value_builtin/3,
    starlog_unregister_value_builtin/2,
    starlog_set_debug/1,
    starlog_show_expansion/1,
    starlog_output_code/1,
    starlog_output_code/2,
    starlog_output_code/3,
    starlog_output_file/1,
    starlog_output_file/2,
    starlog_output_file/3,
    starlog_to_prolog_code/1,
    starlog_to_prolog_code/2,
    starlog_to_prolog_code/3,
    starlog_to_prolog_file/1,
    starlog_to_prolog_file/2,
    starlog_to_prolog_file/3
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

% ============================================================
% Variable Renaming Utilities
% ============================================================

% generate_var_name(+Index, -Name)
% Generate a variable name from an index (0 -> A, 1 -> B, ..., 26 -> A1, etc.)
generate_var_name(Index, Name) :-
    Index < 26,
    !,
    Code is 65 + Index,  % 65 is ASCII for 'A'
    atom_codes(Name, [Code]).
generate_var_name(Index, Name) :-
    Cycle is Index // 26,
    Offset is Index mod 26,
    Code is 65 + Offset,
    atom_codes(BaseName, [Code]),
    atom_concat(BaseName, Cycle, Name).

% rename_variables(+Term, -RenamedTerm)
% Rename all variables in a term to human-friendly names (A, B, C, ..., A1, B1, ...)
rename_variables(Term, RenamedTerm) :-
    term_variables(Term, Vars),
    rename_vars_list(Vars, 0),
    copy_term(Term, RenamedTerm).

% rename_vars_list(+Vars, +StartIndex)
% Rename a list of variables starting from StartIndex
% Note: Uses SWI-Prolog's '$VAR'/1 special term for variable naming with numbervars/3.
% This is a standard SWI-Prolog feature for pretty-printing variables with readable names.
rename_vars_list([], _).
rename_vars_list([Var|Vars], Index) :-
    generate_var_name(Index, Name),
    Var = '$VAR'(Name),
    NextIndex is Index + 1,
    rename_vars_list(Vars, NextIndex).

% ============================================================
% Starlog Code Output Predicates
% ============================================================

% starlog_output_code(+Goal)
% Output the Starlog code representation of a goal with human-friendly variable names.
% This shows how a Prolog goal would be written in Starlog notation.
starlog_output_code(Goal) :-
    starlog_output_code(Goal, _).

% starlog_output_code(+Goal, -StarlogCode)
% Output the Starlog code representation of a goal and return it as a term.
starlog_output_code(Goal, StarlogCode) :-
    starlog_output_code(Goal, StarlogCode, []).

% starlog_output_code(+Goal, -StarlogCode, +Options)
% Output the Starlog code representation with options.
% Options:
%   compress(true) - Apply maximal compression by nesting expressions
%   compress(false) - No compression (default)
starlog_output_code(Goal, StarlogCode, Options) :-
    % First, check if it's already a Starlog expression
    (is_already_starlog(Goal) ->
        % If it's already Starlog, just rename variables and output
        rename_variables(Goal, RenamedGoal),
        StarlogCode = RenamedGoal,
        write_term(StarlogCode, [numbervars(true), quoted(true)]), nl
    ;
        % Otherwise, convert Prolog to Starlog
        convert_prolog_to_starlog(Goal, StarlogForm),
        % Apply compression if requested
        (member(compress(true), Options) ->
            compress_starlog(StarlogForm, CompressedForm)
        ;
            CompressedForm = StarlogForm
        ),
        rename_variables(CompressedForm, RenamedStarlog),
        StarlogCode = RenamedStarlog,
        write_term(StarlogCode, [numbervars(true), quoted(true)]), nl
    ).

% is_already_starlog(+Goal)
% Check if a goal is already in Starlog form (contains 'is' with operators)
is_already_starlog(_ is Expr) :-
    contains_starlog_op(Expr).
is_already_starlog(_) :- fail.

% contains_starlog_op(+Expr)
% Check if expression contains Starlog operators
contains_starlog_op(_ : _) :- !.
contains_starlog_op(_ & _) :- !.
contains_starlog_op(_ • _) :- !.
contains_starlog_op(Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    member(Arg, Args),
    contains_starlog_op(Arg).

% convert_prolog_to_starlog(+PrologGoal, -StarlogForm)
% Convert a Prolog goal to Starlog notation.
% This is the inverse of the expansion process.
convert_prolog_to_starlog((Goal, Rest), (StarlogGoal, StarlogRest)) :-
    !,
    convert_prolog_to_starlog(Goal, StarlogGoal),
    convert_prolog_to_starlog(Rest, StarlogRest).

% String concatenation
convert_prolog_to_starlog(string_concat(A, B, C), (C is (A:B))) :- !.

% List append
convert_prolog_to_starlog(append(A, B, C), (C is (A&B))) :- !.

% Atom concatenation
convert_prolog_to_starlog(atom_concat(A, B, C), (C is (A•B))) :- !.

% Value-returning builtins
% Convert Prolog predicates back to Starlog 'is' notation.
% For example: reverse([1,2,3], R) -> R is reverse([1,2,3])
% The predicate is_value_builtin(Name, Arity, PrologPred) is used in reverse:
% we have the PrologPred and want to find the Name (BaseName) to use in Starlog.
convert_prolog_to_starlog(Goal, (Out is Func)) :-
    Goal =.. [Pred|Args],
    Args \= [],
    append(InArgs, [Out], Args),
    InArgs \= [],
    starlog_registry:is_value_builtin(BaseName, Arity, Pred),
    length(InArgs, Arity),
    !,
    (InArgs = [] -> Func = BaseName ; Func =.. [BaseName|InArgs]).

% Nullary value-returning builtins
convert_prolog_to_starlog(Goal, (Out is Func)) :-
    Goal =.. [Pred, Out],
    starlog_registry:is_value_builtin(Func, 0, Pred),
    !.

% Default: keep as is
convert_prolog_to_starlog(Goal, Goal).

% ============================================================
% Starlog Compression
% ============================================================
% This section implements maximal compression of Starlog expressions.
% The algorithm nests expressions by replacing intermediate variables
% with their defining expressions when the variable is used exactly once.
% This creates more concise, functional-style code.
%
% Key features:
% - Iterative compression until no more nesting is possible
% - Preserves variables used multiple times
% - Excludes control structures (if-then, or, not) from nesting
% - Prevents circular references

% compress_starlog(+StarlogForm, -CompressedForm)
% Maximally compress Starlog code by nesting expressions where possible.
% Excludes: if-then clauses, logical control structures (or, not),
% and calls without an output that is another's input.
compress_starlog(StarlogForm, CompressedForm) :-
    % Collect all goals from conjunction
    collect_goals(StarlogForm, Goals),
    % Compress by iteratively nesting goals
    compress_goals_iterative(Goals, CompressedGoals),
    % Convert back to conjunction
    goals_to_conjunction(CompressedGoals, CompressedForm).

% collect_goals(+Conjunction, -Goals)
% Collect all goals from a conjunction into a list
collect_goals((A, B), Goals) :-
    !,
    collect_goals(A, GoalsA),
    collect_goals(B, GoalsB),
    append(GoalsA, GoalsB, Goals).
collect_goals(Goal, [Goal]).

% compress_goals_iterative(+Goals, -CompressedGoals)
% Iteratively compress goals until no more compression is possible
compress_goals_iterative(Goals, CompressedGoals) :-
    compress_one_pass(Goals, IntermediateGoals, Changed),
    (Changed = true ->
        compress_goals_iterative(IntermediateGoals, CompressedGoals)
    ;
        CompressedGoals = Goals
    ).

% compress_one_pass(+Goals, -CompressedGoals, -Changed)
% Try to compress one variable in the goal list
compress_one_pass(Goals, CompressedGoals, Changed) :-
    compress_one_pass_helper(Goals, [], CompressedGoals, Changed).

% compress_one_pass_helper(+RemainingGoals, +ProcessedGoals, -Result, -Changed)
compress_one_pass_helper([], Processed, Result, false) :-
    reverse(Processed, Result).
compress_one_pass_helper([Goal|Rest], Processed, Result, Changed) :-
    % Try to nest this goal into a later goal
    (try_nest_goal(Goal, Rest, NewRest) ->
        % Successfully nested - discard Goal and use NewRest
        % Combine processed goals with the modified rest
        reverse(Processed, RevProcessed),
        append(RevProcessed, NewRest, AllGoals),
        Result = AllGoals,
        Changed = true
    ;
        % Cannot nest - move to processed and continue
        compress_one_pass_helper(Rest, [Goal|Processed], Result, Changed)
    ).

% try_nest_goal(+Goal, +LaterGoals, -NewLaterGoals)
% Try to nest Goal into one of the LaterGoals.
% Only nest if the output variable is used exactly once in later goals
% and the expression is not a control structure.
try_nest_goal(Goal, LaterGoals, NewLaterGoals) :-
    % Goal must be: Var is Expr, and Expr must not be a control structure
    Goal = (OutVar is Expr),
    \+ is_control_structure(Expr),
    !,
    % Count how many times OutVar is used in LaterGoals
    count_var_uses(OutVar, LaterGoals, UseCount),
    % Only nest if used exactly once
    UseCount =:= 1,
    % Find and replace that one use
    find_and_replace_use(OutVar, Expr, LaterGoals, NewLaterGoals).

% count_var_uses(+Var, +Goals, -Count)
% Count how many goals use Var as input
count_var_uses(Var, Goals, Count) :-
    findall(1, (member(Goal, Goals), uses_var_as_input(Goal, Var)), Uses),
    length(Uses, Count).

% find_and_replace_use(+Var, +Replacement, +Goals, -NewGoals)
% Find the first goal that uses Var and replace Var with Replacement.
% Direct substitution is used (no copy_term) to preserve variable bindings
% across the entire goal structure.
find_and_replace_use(Var, Replacement, [Goal|Rest], [NewGoal|Rest]) :-
    % Check if Goal uses Var and is not a control structure
    \+ is_control_goal(Goal),
    uses_var_as_input(Goal, Var),
    % Check that Var is not the output of this goal (would create circular reference)
    \+ is_output_of_goal(Goal, Var),
    % Substitute Var with Replacement (no copy_term - work with original)
    substitute_term(Goal, Var, Replacement, NewGoal),
    !.
find_and_replace_use(Var, Replacement, [Goal|Rest], [Goal|NewRest]) :-
    find_and_replace_use(Var, Replacement, Rest, NewRest).

% uses_var_as_input(+Goal, +Var)
% Check if Goal uses Var as an input (not as output)
uses_var_as_input((_ is Expr), Var) :-
    !,
    contains_var(Expr, Var).
uses_var_as_input(Goal, Var) :-
    \+ (Goal = (_ is _)),
    contains_var(Goal, Var).

% is_output_of_goal(+Goal, +Var)
% Check if Var is the output of Goal
is_output_of_goal((V is _), Var) :-
    V == Var.

% contains_var(+Term, +Var)
% Check if Term contains Var
contains_var(Term, Var) :-
    Term == Var, !.
contains_var(Term, Var) :-
    compound(Term),
    Term =.. [_|Args],
    member(Arg, Args),
    contains_var(Arg, Var).

% is_control_structure(+Expr)
% Check if expression is a control structure
is_control_structure((_;_)) :- !.
is_control_structure((_->_)) :- !.
is_control_structure((_->_;_)) :- !.
is_control_structure(\+(_)) :- !.
is_control_structure(not(_)) :- !.

% is_control_goal(+Goal)
% Check if goal is a control structure
is_control_goal((_;_)) :- !.
is_control_goal((_->_)) :- !.
is_control_goal((_->_;_)) :- !.
is_control_goal(\+(_)) :- !.
is_control_goal(not(_)) :- !.
is_control_goal(!).

% substitute_term(+Term, +Old, +New, -Result)
% Substitute all occurrences of Old with New in Term
substitute_term(Term, Old, New, Result) :-
    Term == Old,
    !,
    Result = New.
substitute_term(Term, _Old, _New, Term) :-
    var(Term),
    !.
substitute_term(Term, Old, New, Result) :-
    compound(Term),
    !,
    Term =.. [Functor|Args],
    maplist(substitute_term_arg(Old, New), Args, NewArgs),
    Result =.. [Functor|NewArgs].
substitute_term(Term, _Old, _New, Term).

% substitute_term_arg(+Old, +New, +Arg, -NewArg)
% Helper for substituting in arguments
substitute_term_arg(Old, New, Arg, NewArg) :-
    substitute_term(Arg, Old, New, NewArg).

% starlog_output_file(+FilePath)
% Output Starlog code representation for all clauses in a file.
starlog_output_file(FilePath) :-
    starlog_output_file(FilePath, user_output).

% starlog_output_file(+FilePath, +OutputStream)
% Output Starlog code representation for all clauses in a file to a stream.
starlog_output_file(FilePath, OutputStream) :-
    starlog_output_file(FilePath, OutputStream, []).

% starlog_output_file(+FilePath, +OutputStream, +Options)
% Output Starlog code representation for all clauses in a file to a stream with options.
% Options:
%   compress(true) - Apply maximal compression by nesting expressions
%   compress(false) - No compression (default)
starlog_output_file(FilePath, OutputStream, Options) :-
    format(OutputStream, '% Starlog code output for file: ~w~n~n', [FilePath]),
    setup_call_cleanup(
        open(FilePath, read, Stream),
        read_and_output_clauses(Stream, OutputStream, Options),
        close(Stream)
    ).

% read_and_output_clauses(+InputStream, +OutputStream, +Options)
% Read clauses from input stream and output their Starlog representation.
read_and_output_clauses(InputStream, OutputStream, Options) :-
    read_term(InputStream, Term, []),
    (Term == end_of_file ->
        true
    ;
        output_clause_as_starlog(Term, OutputStream, Options),
        read_and_output_clauses(InputStream, OutputStream, Options)
    ).

% output_clause_as_starlog(+Clause, +OutputStream, +Options)
% Output a single clause in Starlog notation.
output_clause_as_starlog((:- Directive), OutputStream, _Options) :-
    !,
    % Directives are output as-is
    write_term(OutputStream, (:- Directive), [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_starlog((?- Query), OutputStream, _Options) :-
    !,
    % Queries are output as-is
    write_term(OutputStream, (?- Query), [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_starlog((Head :- Body), OutputStream, Options) :-
    !,
    % Convert body to Starlog, keep head as is
    convert_prolog_to_starlog(Body, StarlogBody),
    % Apply compression if requested
    (member(compress(true), Options) ->
        compress_starlog(StarlogBody, CompressedBody)
    ;
        CompressedBody = StarlogBody
    ),
    rename_variables((Head :- CompressedBody), RenamedClause),
    write_term(OutputStream, RenamedClause, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_starlog(Fact, OutputStream, _Options) :-
    % Facts are output as-is
    rename_variables(Fact, RenamedFact),
    write_term(OutputStream, RenamedFact, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

% ============================================================
% Starlog to Prolog Conversion (Maximal Decompression)
% ============================================================
% This section implements conversion from Starlog syntax to standard Prolog.
% The algorithm decompresses nested expressions into sequential goals,
% using human-friendly variable names (A, B, C, ..., A1, B1, ...).
%
% Key features:
% - Expands Starlog operators (: & •) to standard Prolog predicates
% - Flattens nested expressions into sequential goals
% - Uses human-friendly variable naming
% - Handles both individual goals and entire files

% starlog_to_prolog_code(+StarlogGoal)
% Convert a Starlog goal to Prolog code with maximal decompression.
% Outputs the Prolog code with human-friendly variable names.
starlog_to_prolog_code(StarlogGoal) :-
    starlog_to_prolog_code(StarlogGoal, _).

% starlog_to_prolog_code(+StarlogGoal, -PrologCode)
% Convert a Starlog goal to Prolog code and return it as a term.
starlog_to_prolog_code(StarlogGoal, PrologCode) :-
    starlog_to_prolog_code(StarlogGoal, PrologCode, []).

% starlog_to_prolog_code(+StarlogGoal, -PrologCode, +Options)
% Convert a Starlog goal to Prolog code with options.
% Options:
%   decompress(true) - Apply maximal decompression (default)
%   decompress(false) - Minimal decompression
starlog_to_prolog_code(StarlogGoal, PrologCode, Options) :-
    % Expand Starlog to Prolog goals
    starlog_expand:expand_starlog_goal(StarlogGoal, ExpandedGoal),
    % Apply decompression if requested (default is true)
    (\+ member(decompress(false), Options) ->
        % Decompression is already done by expand_starlog_goal
        % which flattens nested expressions
        DecompressedGoal = ExpandedGoal
    ;
        DecompressedGoal = ExpandedGoal
    ),
    % Apply human-friendly variable renaming
    rename_variables(DecompressedGoal, RenamedGoal),
    PrologCode = RenamedGoal,
    % Output the result
    write_term(PrologCode, [numbervars(true), quoted(true)]), nl.

% starlog_to_prolog_file(+FilePath)
% Convert a Starlog file to Prolog code with maximal decompression.
starlog_to_prolog_file(FilePath) :-
    starlog_to_prolog_file(FilePath, user_output).

% starlog_to_prolog_file(+FilePath, +OutputStream)
% Convert a Starlog file to Prolog code and write to a stream.
starlog_to_prolog_file(FilePath, OutputStream) :-
    starlog_to_prolog_file(FilePath, OutputStream, []).

% starlog_to_prolog_file(+FilePath, +OutputStream, +Options)
% Convert a Starlog file to Prolog code with options.
% Options:
%   decompress(true) - Apply maximal decompression (default)
%   decompress(false) - Minimal decompression
starlog_to_prolog_file(FilePath, OutputStream, Options) :-
    format(OutputStream, '% Prolog code output for file: ~w~n~n', [FilePath]),
    setup_call_cleanup(
        open(FilePath, read, Stream),
        read_and_output_prolog_clauses(Stream, OutputStream, Options),
        close(Stream)
    ).

% read_and_output_prolog_clauses(+InputStream, +OutputStream, +Options)
% Read clauses from input stream and output their Prolog representation.
read_and_output_prolog_clauses(InputStream, OutputStream, Options) :-
    read_term(InputStream, Term, []),
    (Term == end_of_file ->
        true
    ;
        output_clause_as_prolog(Term, OutputStream, Options),
        read_and_output_prolog_clauses(InputStream, OutputStream, Options)
    ).

% output_clause_as_prolog(+Clause, +OutputStream, +Options)
% Output a single clause in Prolog notation.
output_clause_as_prolog((:- Directive), OutputStream, _Options) :-
    !,
    % Directives are output as-is
    write_term(OutputStream, (:- Directive), [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_prolog((?- Query), OutputStream, _Options) :-
    !,
    % Queries are output as-is
    write_term(OutputStream, (?- Query), [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_prolog((Head :- Body), OutputStream, _Options) :-
    !,
    % Expand Starlog body to Prolog
    starlog_expand:expand_starlog_goal(Body, ExpandedBody),
    % Decompression is already done by expand_starlog_goal
    DecompressedBody = ExpandedBody,
    % Apply human-friendly variable renaming
    rename_variables((Head :- DecompressedBody), RenamedClause),
    write_term(OutputStream, RenamedClause, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_prolog(Fact, OutputStream, _Options) :-
    % Facts are output as-is
    rename_variables(Fact, RenamedFact),
    write_term(OutputStream, RenamedFact, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).
