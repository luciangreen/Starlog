% starlog.pl 
% Main library for Starlog-in-Prolog
% Allows developers to write Starlog syntax directly in Prolog files
% and have it expanded automatically at load-time.

:- module(starlog, [
    starlog_call/1,
    starlog_call/2,
    starlog_eval/2,
    starlog_no_eval/2,
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
    starlog_to_prolog_file/3,
    solve_equation/2,
    solve_equation/3,
    solve_system/2,
    solve_system/3,
    gaussian_elimination/2,
    gaussian_elimination/3,
    find/3
]).

:- use_module(starlog_expand).
:- use_module(starlog_registry).
:- use_module(algebra_solver).
:- use_module(gaussian_elimination).

% Define helper predicates in user module
:- multifile user:string_number/2.
:- dynamic user:string_number/2.

% string_number(?String, ?Number)
% Convert between string and number (wrapper for number_string with swapped args)
user:string_number(String, Number) :-
    number_string(Number, String).

% foldr(?Operation, ?List, ?Accumulator, ?Result)
% Fold a list from right to left using the given binary operation.
% The operation can be a predicate name (atom) or a callable term.
% For value-returning operations in Starlog, use the predicate name.
% Example: foldr(string_concat, [c, b, a], "", Result) -> Result = "cba"
%   (processes 'a' first, then 'b', then 'c': concat('c', concat('b', concat('a', ""))))
% Example: foldr(append, [[1], [2], [3]], [], Result) -> Result = [1,2,3]
%   (processes [3] first, then [2], then [1]: append([1], append([2], append([3], []))))
% Note: This implementation uses structural recursion which is appropriate for foldr semantics.
% For very large lists (10,000+ elements), consider using foldl instead if order doesn't matter.
:- multifile user:foldr/4.
:- dynamic user:foldr/4.

user:foldr(_, [], Acc, Acc).
user:foldr(Op, [H|T], Acc, Result) :-
    user:foldr(Op, T, Acc, AccNext),
    % Apply the operation: Op(H, AccNext, Result)
    % Handle both atom operation names and compound terms
    (atom(Op) ->
        Goal =.. [Op, H, AccNext, Result]
    ;
        % If Op is a compound term, add the arguments
        Op =.. OpList,
        append(OpList, [H, AccNext, Result], FullArgs),
        Goal =.. FullArgs
    ),
    call(Goal).

% Define Starlog operators globally (in user module)
:- op(700, xfx, user:(is)).
:- op(650, yfx, user:(>>)).    % Method chain operator (pipe forward)
:- op(600, yfx, user:(':' )).
:- op(600, yfx, user:('&')).
:- op(600, yfx, user:('•')).
:- op(600, fx, user:('..=')).  % List to term conversion
:- op(600, fx, user:('=..')).  % Term to list conversion (unary prefix)

% Install portray hook for proper 'is' operator display
% This ensures that expressions like (X is Y) are displayed with proper spacing
% even when X is a quoted string or atom
:- multifile user:portray/1.

user:portray(is(Left, Right)) :-
    % Only portray if Left is a string or atom that needs quoting
    (string(Left); atom(Left)),
    % Write the term with explicit spacing using print for proper operator display
    (string(Left) -> 
        % Strings are always quoted with double quotes
        (write('"'), write(Left), write('"'))
    ; 
        % Atoms may need quoting with single quotes if they contain special characters
        % Use writeq to handle proper atom quoting
        writeq(Left)
    ),
    write(' is '),
    print(Right),
    !.

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

% Install call hook to support runtime goal expansion
% This allows variable-bound Starlog goals to work correctly when called via call/1
:- multifile prolog:call_hook/2.

prolog:call_hook(Goal, ExpandedGoal) :-
    % Quick check: only try to expand if Goal looks like it might be a Starlog expression
    % This avoids the overhead of attempted expansion for normal Prolog goals
    (var(Goal) ; compound(Goal)),
    might_be_starlog_goal(Goal),
    % Try to expand as a Starlog goal
    starlog_expand:expand_starlog_goal(Goal, Expanded),
    Goal \== Expanded,
    !,
    % Goal was expanded - return the expanded version
    ExpandedGoal = Expanded.

% might_be_starlog_goal(+Goal)
% Quick preliminary check to filter out obviously non-Starlog goals
% This improves performance by avoiding expensive expansion attempts on normal Prolog goals
might_be_starlog_goal(Goal) :-
    var(Goal),  % Variables might contain Starlog goals
    !.
might_be_starlog_goal(_ is _) :- !.  % Contains 'is' - might be Starlog
might_be_starlog_goal((_ , _)) :- !.  % Conjunction - might contain Starlog
might_be_starlog_goal((_ ; _)) :- !.  % Disjunction - might contain Starlog
might_be_starlog_goal((_ -> _)) :- !.  % If-then - might contain Starlog
might_be_starlog_goal(\+ _) :- !.  % Negation - might contain Starlog
% Note: We err on the side of caution - if we're not sure, we say it might be Starlog
% The actual expansion will fail quickly if it's not really Starlog

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

% starlog_call(+Goal, -Result)
% Execute a Starlog goal and explicitly return the result.
% This is useful when you want to save the result of a Prolog or Starlog
% call to a variable. The goal should be of the form 'Var is Expr'.
% Example: starlog_call(X is "hello":"world", Result) binds Result to "helloworld"
starlog_call((Var is Expr), Result) :-
    !,
    starlog_expand:expand_starlog_goal((Var is Expr), ExpandedGoal),
    call(ExpandedGoal),
    Result = Var.
starlog_call(Goal, _Result) :-
    throw(error(type_error(starlog_is_expression, Goal), 
                context(starlog_call/2, 'Goal must be of the form: Var is Expr'))).

% starlog_eval(+Expr, -Result)
% Evaluate a Starlog expression and return the result.
% This forces evaluation of the expression, similar to wrapping it in eval().
% Example: starlog_eval("x":"y", Result) binds Result to "xy"
% Example: starlog_eval(1+1, Result) binds Result to 2
starlog_eval(Expr, Result) :-
    starlog_call((Result is eval(Expr)), Result).

% starlog_no_eval(+Expr, -Result)
% Preserve a Starlog expression without evaluation and return it.
% This prevents evaluation of the expression, similar to wrapping it in no_eval().
% Example: starlog_no_eval("x":"y", Result) binds Result to "x":"y" (not "xy")
% Example: starlog_no_eval(1+1, Result) binds Result to 1+1 (not 2)
starlog_no_eval(Expr, Result) :-
    starlog_call((Result is no_eval(Expr)), Result).

% find(+Template, +Goal, -Result)
% Execute a goal with a cut and collect the first solution.
% If the template is a Starlog expression, it will be evaluated.
% This is equivalent to: findall(Template, (Goal, !), [Collected]), then evaluate if needed
% Example: find(A, [A:a] is [a:a], Result) binds Result to atom 'a'
% Example: find(X, member(X, [1,2,3]), Result) binds Result to 1
% Example: find(A:C, starlog_call([A:d:a:C] is [a:d:a:c]), Result) binds Result to "ac"
find(A, B, C) :- 
    findall(A, (B, !), [Collected]),
    % Try to evaluate the template as a Starlog expression
    % If it succeeds, use the evaluated result; otherwise use the original
    % Only catch specific exceptions that indicate evaluation isn't applicable
    (catch(starlog_eval(Collected, Evaluated), 
           error(E, _),  % Catch specific error terms
           (member(E, [type_error(_,_), instantiation_error]), fail)) ->
        C = Evaluated
    ;
        C = Collected
    ).

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
    starlog_output_code(Goal, _, [print(true)]).

% starlog_output_code(+Goal, -StarlogCode)
% Get the Starlog code representation of a goal and return it as a term.
% Does not print to stdout - only returns the code in the variable.
starlog_output_code(Goal, StarlogCode) :-
    starlog_output_code(Goal, StarlogCode, [print(false)]).

% starlog_output_code(+Goal, -StarlogCode, +Options)
% Output the Starlog code representation with options.
% Options:
%   compress(true) - Apply maximal compression by nesting expressions
%   compress(false) - No compression (default)
%   output_eval(true) - Keep eval() wrappers in output
%   output_eval(false) - Strip eval() wrappers (default)
%   output_no_eval(true) - Keep no_eval() wrappers in output
%   output_no_eval(false) - Strip no_eval() wrappers (default)
%   output_style(nested_calls) - Convert method chains to nested calls
%   output_style(method_chaining) - Convert nested calls to method chains
%   print(true) - Print to stdout (default for /1 version)
%   print(false) - Do not print to stdout (default for /2 version)
%   rename(true) - Rename variables to A, B, C, etc. (default for print)
%   rename(false) - Keep original variables (default for no print)
starlog_output_code(Goal, StarlogCode, Options) :-
    % Determine if we should rename variables
    % Default: rename when printing, don't rename otherwise
    determine_rename_option(Options, Rename),
    % First, check if it's already a Starlog expression
    (is_already_starlog(Goal) ->
        % If it's already Starlog, optionally rename variables and output
        apply_rename(Goal, Rename, ProcessedGoal),
        % Strip eval/no_eval based on options
        strip_eval_no_eval_based_on_options(ProcessedGoal, Options, StrippedGoal),
        StarlogCode0 = StrippedGoal
    ;
        % Otherwise, convert Prolog to Starlog
        convert_prolog_to_starlog(Goal, StarlogForm),
        % Apply compression if requested
        (member(compress(true), Options) ->
            compress_starlog(StarlogForm, CompressedForm)
        ;
            CompressedForm = StarlogForm
        ),
        % Optionally rename variables
        apply_rename(CompressedForm, Rename, ProcessedStarlog),
        % Strip eval/no_eval based on options
        strip_eval_no_eval_based_on_options(ProcessedStarlog, Options, StrippedStarlog),
        StarlogCode0 = StrippedStarlog
    ),
    % Apply output_style transformation if specified
    (member(output_style(Style), Options) ->
        apply_output_style(StarlogCode0, Style, StarlogCode)
    ;
        StarlogCode = StarlogCode0
    ),
    % Print to stdout only if print(true) option is present
    (member(print(true), Options) ->
        pretty_write_body(StarlogCode, user_output, 0), nl
    ;
        true
    ).

% determine_rename_option(+Options, -Rename)
% Determine whether to rename variables based on options
determine_rename_option(Options, Rename) :-
    (member(rename(Rename), Options) ->
        true  % Use explicit rename option
    ; member(print(true), Options) ->
        Rename = true  % Default to rename when printing
    ;
        Rename = false  % Default to not rename otherwise
    ).

% apply_rename(+Term, +Rename, -RenamedTerm)
% Apply variable renaming if Rename is true
apply_rename(Term, true, RenamedTerm) :-
    !,
    rename_variables(Term, RenamedTerm).
apply_rename(Term, false, Term).

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
contains_starlog_op(..=(_)) :- !.
contains_starlog_op(=..(_)) :- !.
contains_starlog_op(eval(_)) :- !.
contains_starlog_op(no_eval(_)) :- !.
contains_starlog_op(Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    member(Arg, Args),
    contains_starlog_op(Arg).

% strip_eval_no_eval_based_on_options(+Term, +Options, -StrippedTerm)
% Strip eval() and no_eval() wrappers based on options.
% By default (if options not specified), both are stripped.
strip_eval_no_eval_based_on_options(Term, Options, StrippedTerm) :-
    % Check options - default is to strip both eval and no_eval
    (member(output_eval(true), Options) -> StripEval = false ; StripEval = true),
    (member(output_no_eval(true), Options) -> StripNoEval = false ; StripNoEval = true),
    strip_eval_no_eval(Term, StripEval, StripNoEval, StrippedTerm).

% strip_eval_no_eval(+Term, +StripEval, +StripNoEval, -StrippedTerm)
% Strip eval() and/or no_eval() wrappers from a term.
% StripEval: true to remove eval() wrappers, false to keep them
% StripNoEval: true to remove no_eval() wrappers, false to keep them

% Handle variables - return as-is
strip_eval_no_eval(Term, _StripEval, _StripNoEval, Term) :-
    var(Term),
    !.

% Handle conjunction
strip_eval_no_eval((A, B), StripEval, StripNoEval, (StrippedA, StrippedB)) :-
    !,
    strip_eval_no_eval(A, StripEval, StripNoEval, StrippedA),
    strip_eval_no_eval(B, StripEval, StripNoEval, StrippedB).

% Handle disjunction
strip_eval_no_eval((A ; B), StripEval, StripNoEval, (StrippedA ; StrippedB)) :-
    !,
    strip_eval_no_eval(A, StripEval, StripNoEval, StrippedA),
    strip_eval_no_eval(B, StripEval, StripNoEval, StrippedB).

% Handle if-then-else
strip_eval_no_eval((A -> B ; C), StripEval, StripNoEval, (StrippedA -> StrippedB ; StrippedC)) :-
    !,
    strip_eval_no_eval(A, StripEval, StripNoEval, StrippedA),
    strip_eval_no_eval(B, StripEval, StripNoEval, StrippedB),
    strip_eval_no_eval(C, StripEval, StripNoEval, StrippedC).

% Handle if-then
strip_eval_no_eval((A -> B), StripEval, StripNoEval, (StrippedA -> StrippedB)) :-
    !,
    strip_eval_no_eval(A, StripEval, StripNoEval, StrippedA),
    strip_eval_no_eval(B, StripEval, StripNoEval, StrippedB).

% Handle negation
strip_eval_no_eval(\+ A, StripEval, StripNoEval, \+ StrippedA) :-
    !,
    strip_eval_no_eval(A, StripEval, StripNoEval, StrippedA).

% Handle 'is' expressions
strip_eval_no_eval((Out is Expr), StripEval, StripNoEval, (Out is StrippedExpr)) :-
    !,
    strip_eval_no_eval(Expr, StripEval, StripNoEval, StrippedExpr).

% Strip eval() wrapper if requested
% When stripping eval(), we need to actually evaluate the expression
strip_eval_no_eval(eval(Inner), true, StripNoEval, EvaluatedResult) :-
    !,
    % First evaluate the Inner expression using starlog_call
    (catch(starlog_call(TempResult is Inner), _, fail) ->
        % Successfully evaluated - use the result
        EvaluatedResult = TempResult
    ;
        % If evaluation fails, just strip recursively without evaluation
        strip_eval_no_eval(Inner, true, StripNoEval, EvaluatedResult)
    ).

% Keep eval() wrapper if requested
strip_eval_no_eval(eval(Inner), false, StripNoEval, eval(StrippedInner)) :-
    !,
    strip_eval_no_eval(Inner, false, StripNoEval, StrippedInner).

% Strip no_eval() wrapper if requested
strip_eval_no_eval(no_eval(Inner), StripEval, true, StrippedInner) :-
    !,
    strip_eval_no_eval(Inner, StripEval, true, StrippedInner).

% Keep no_eval() wrapper if requested
strip_eval_no_eval(no_eval(Inner), StripEval, false, no_eval(StrippedInner)) :-
    !,
    strip_eval_no_eval(Inner, StripEval, false, StrippedInner).

% Handle compound terms recursively
strip_eval_no_eval(Term, StripEval, StripNoEval, StrippedTerm) :-
    compound(Term),
    !,
    Term =.. [Functor|Args],
    maplist(strip_eval_no_eval_arg(StripEval, StripNoEval), Args, StrippedArgs),
    StrippedTerm =.. [Functor|StrippedArgs].

% Atomic terms - return as-is
strip_eval_no_eval(Term, _StripEval, _StripNoEval, Term).

% strip_eval_no_eval_arg(+StripEval, +StripNoEval, +Arg, -StrippedArg)
% Helper for stripping in arguments
strip_eval_no_eval_arg(StripEval, StripNoEval, Arg, StrippedArg) :-
    strip_eval_no_eval(Arg, StripEval, StripNoEval, StrippedArg).

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

% Univ operator conversions
% Convert Term =.. List to Starlog notation
convert_prolog_to_starlog((Term =.. Out), (Out is =..(Term))) :- !.
convert_prolog_to_starlog((Out =.. List), (Out is ..=(List))) :- !.

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
% Output Style Transformations
% ============================================================
% This section implements transformations between nested calls and method chaining.
% - output_style(nested_calls): Convert method chains to nested function calls
% - output_style(method_chaining): Convert nested function calls to method chains

% apply_output_style(+StarlogCode, +Style, -TransformedCode)
% Apply the specified output style transformation to Starlog code
apply_output_style(Code, nested_calls, TransformedCode) :-
    !,
    transform_chains_to_nested(Code, TransformedCode).
apply_output_style(Code, method_chaining, TransformedCode) :-
    !,
    transform_nested_to_chains(Code, TransformedCode).
apply_output_style(Code, _, Code).  % Unknown style - no transformation

% transform_chains_to_nested(+Code, -TransformedCode)
% Convert method chains (>>) to nested function calls
transform_chains_to_nested((Goal, Rest), (TransformedGoal, TransformedRest)) :-
    !,
    transform_chains_to_nested(Goal, TransformedGoal),
    transform_chains_to_nested(Rest, TransformedRest).
transform_chains_to_nested((Var is Expr), (Var is TransformedExpr)) :-
    !,
    transform_chain_expr_to_nested(Expr, TransformedExpr).
transform_chains_to_nested(Goal, Goal).

% transform_chain_expr_to_nested(+Expr, -NestedExpr)
% Transform a chain expression to nested form
% Example: reverse([1,2,3]) >> length  =>  length(reverse([1,2,3]))
transform_chain_expr_to_nested((Base >> Method), NestedExpr) :-
    !,
    % First, transform the base (it might contain chains too)
    transform_chain_expr_to_nested(Base, TransformedBase),
    % Collect all methods in the chain
    collect_chain_methods(Method, Methods),
    % Build nested calls from inside out
    build_nested_from_chain(Methods, TransformedBase, NestedExpr).
% Handle operators that might contain chains
transform_chain_expr_to_nested((A : B), (TransA : TransB)) :-
    !,
    transform_chain_expr_to_nested(A, TransA),
    transform_chain_expr_to_nested(B, TransB).
transform_chain_expr_to_nested((A & B), (TransA & TransB)) :-
    !,
    transform_chain_expr_to_nested(A, TransA),
    transform_chain_expr_to_nested(B, TransB).
transform_chain_expr_to_nested((A • B), (TransA • TransB)) :-
    !,
    transform_chain_expr_to_nested(A, TransA),
    transform_chain_expr_to_nested(B, TransB).
% Handle compound terms (function calls)
transform_chain_expr_to_nested(Func, TransformedFunc) :-
    compound(Func),
    \+ is_operator_expr(Func),
    !,
    Func =.. [Functor|Args],
    transform_chain_expr_list(Args, TransformedArgs),
    TransformedFunc =.. [Functor|TransformedArgs].
% Default: atoms, numbers, etc.
transform_chain_expr_to_nested(Expr, Expr).

% transform_chain_expr_list(+List, -TransformedList)
transform_chain_expr_list([], []).
transform_chain_expr_list([H|T], [TransH|TransT]) :-
    transform_chain_expr_to_nested(H, TransH),
    transform_chain_expr_list(T, TransT).

% is_operator_expr(+Expr)
% Check if expression is a Starlog operator
is_operator_expr((_ : _)).
is_operator_expr((_ & _)).
is_operator_expr((_ • _)).
is_operator_expr((_ >> _)).

% collect_chain_methods(+ChainExpr, -Methods)
% Collect methods from a chain expression into a list
% Input: a >> b >> c (parsed as a >> (b >> c))
% Output: [a, b, c]
collect_chain_methods((Method >> Rest), [Method|MoreMethods]) :-
    !,
    collect_chain_methods(Rest, MoreMethods).
collect_chain_methods(Method, [Method]).

% build_nested_from_chain(+Methods, +Base, -NestedExpr)
% Build nested function calls from a list of methods
% [a, b, c] with base X => c(b(a(X)))
build_nested_from_chain([], Base, Base) :- !.
build_nested_from_chain([Method|Rest], Base, NestedExpr) :-
    % Apply the first method to the base
    apply_method_to_expr(Method, Base, Applied),
    % Recursively build with remaining methods
    build_nested_from_chain(Rest, Applied, NestedExpr).

% apply_method_to_expr(+Method, +Base, -Result)
% Apply a method to a base expression
% Method can be an atom (like 'length') or a compound term (like 'foo(1,2)')
apply_method_to_expr(Method, Base, Result) :-
    atom(Method),
    !,
    Result =.. [Method, Base].
apply_method_to_expr(Method, Base, Result) :-
    compound(Method),
    !,
    Method =.. [Functor|Args],
    append(Args, [Base], NewArgs),
    Result =.. [Functor|NewArgs].

% transform_nested_to_chains(+Code, -TransformedCode)
% Convert nested function calls to method chains
transform_nested_to_chains((Goal, Rest), (TransformedGoal, TransformedRest)) :-
    !,
    transform_nested_to_chains(Goal, TransformedGoal),
    transform_nested_to_chains(Rest, TransformedRest).
transform_nested_to_chains((Var is Expr), (Var is TransformedExpr)) :-
    !,
    transform_nested_expr_to_chain(Expr, TransformedExpr).
transform_nested_to_chains(Goal, Goal).

% transform_nested_expr_to_chain(+Expr, -ChainExpr)
% Transform nested function calls to method chain form
% Example: length(reverse([1,2,3]))  =>  reverse([1,2,3]) >> length
transform_nested_expr_to_chain(Func, ChainExpr) :-
    compound(Func),
    \+ is_operator_expr(Func),
    can_be_chain_method(Func),
    Func =.. [Functor|Args],
    Args \= [],
    last(Args, LastArg),
    % Check if the last argument is also a chainable function OR an operator expression
    (   (compound(LastArg), \+ is_list(LastArg), can_be_chain_method(LastArg)) ->
        % It's a nested chainable function - recurse on the last argument
        transform_nested_expr_to_chain(LastArg, ChainBase),
        % Remove the last argument and create method
        append(InitArgs, [_], Args),
        (InitArgs = [] ->
            Method = Functor
        ;
            Method =.. [Functor|InitArgs]
        ),
        ChainExpr = (ChainBase >> Method)
    ;   (compound(LastArg), is_operator_expr(LastArg)) ->
        % Last arg is an operator expression - make it the chain base
        % Transform the operator expression first
        transform_nested_expr_to_chain(LastArg, ChainBase),
        % Remove the last argument and create method
        append(InitArgs, [_], Args),
        (InitArgs = [] ->
            Method = Functor
        ;
            Method =.. [Functor|InitArgs]
        ),
        ChainExpr = (ChainBase >> Method)
    ;
        % Try to transform arguments that might contain chains
        transform_chain_expr_list(Args, TransformedArgs),
        ChainExpr =.. [Functor|TransformedArgs]
    ).
% Handle operators that might contain nested calls
transform_nested_expr_to_chain((A : B), (TransA : TransB)) :-
    !,
    transform_nested_expr_to_chain(A, TransA),
    transform_nested_expr_to_chain(B, TransB).
transform_nested_expr_to_chain((A & B), (TransA & TransB)) :-
    !,
    transform_nested_expr_to_chain(A, TransA),
    transform_nested_expr_to_chain(B, TransB).
transform_nested_expr_to_chain((A • B), (TransA • TransB)) :-
    !,
    transform_nested_expr_to_chain(A, TransA),
    transform_nested_expr_to_chain(B, TransB).
% Default: atoms, numbers, lists, etc.
transform_nested_expr_to_chain(Expr, Expr).

% can_be_chain_method(+Func)
% Check if a function can be part of a method chain
% Functions with 1+ arguments where the last one could be the input
can_be_chain_method(Func) :-
    compound(Func),
    Func =.. [Functor|Args],
    Args \= [],
    atom(Functor),
    % Check if it's a known value-returning builtin or could be one
    (starlog_registry:is_value_builtin(Functor, _, _) -> true ; true).

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
%   output_eval(true) - Keep eval() wrappers in output
%   output_eval(false) - Strip eval() wrappers (default)
%   output_no_eval(true) - Keep no_eval() wrappers in output
%   output_no_eval(false) - Strip no_eval() wrappers (default)
%   output_style(nested_calls) - Convert method chains to nested calls
%   output_style(method_chaining) - Convert nested calls to method chains
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
    % Strip eval/no_eval based on options
    RenamedClause = (RenamedHead :- RenamedBody),
    strip_eval_no_eval_based_on_options(RenamedBody, Options, StrippedBody),
    % Apply output_style transformation if specified
    (member(output_style(Style), Options) ->
        apply_output_style(StrippedBody, Style, TransformedBody)
    ;
        TransformedBody = StrippedBody
    ),
    % Use pretty printer for the clause
    pretty_write_term_at_level((RenamedHead :- TransformedBody), OutputStream, 0),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_starlog(Fact, OutputStream, _Options) :-
    % Facts are output as-is
    rename_variables(Fact, RenamedFact),
    write_term(OutputStream, RenamedFact, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

% ============================================================
% Pretty Printing with Indentation
% ============================================================
% This section implements pretty printing for Starlog code with proper
% indentation for nested calls and logical control structures.
%
% Key features:
% - Indents nested calls, findall, and, or, not, if-then, if-then-else
% - Maintains proper formatting for complex structures
% - Uses configurable indentation levels

% pretty_write_term(+Term, +OutputStream, +IndentLevel)
% Write a term with pretty printing and indentation.
pretty_write_term(Term, OutputStream, IndentLevel) :-
    write_indent(OutputStream, IndentLevel),
    pretty_write_term_at_level(Term, OutputStream, IndentLevel).

% write_indent(+OutputStream, +Level)
% Write indentation spaces to the output stream.
write_indent(OutputStream, Level) :-
    Spaces is Level * 2,
    write_spaces(OutputStream, Spaces).

% write_spaces(+OutputStream, +Count)
% Write Count spaces to the output stream.
write_spaces(_, 0) :- !.
write_spaces(OutputStream, Count) :-
    Count > 0,
    write(OutputStream, ' '),
    Count1 is Count - 1,
    write_spaces(OutputStream, Count1).

% pretty_write_term_at_level(+Term, +OutputStream, +IndentLevel)
% Write a term with context-aware formatting.
pretty_write_term_at_level((Head :- Body), OutputStream, IndentLevel) :-
    !,
    write_term(OutputStream, Head, [numbervars(true), quoted(true)]),
    write(OutputStream, ':-'),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    pretty_write_body(Body, OutputStream, NextLevel).

% Handle facts
pretty_write_term_at_level(Fact, OutputStream, _IndentLevel) :-
    \+ (Fact = (_ :- _)),
    \+ (Fact = (_ , _)),
    \+ (Fact = (_ ; _)),
    \+ (Fact = (_ -> _)),
    \+ (Fact = (\+ _)),
    !,
    write_term(OutputStream, Fact, [numbervars(true), quoted(true)]).

% pretty_write_body(+Body, +OutputStream, +IndentLevel)
% Write a clause body with proper indentation.

% If-then-else (must come before disjunction)
pretty_write_body((Cond -> Then ; Else), OutputStream, IndentLevel) :-
    !,
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '('),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    pretty_write_goal(Cond, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '->'),
    nl(OutputStream),
    pretty_write_body(Then, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ';'),
    nl(OutputStream),
    pretty_write_body(Else, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ')').

% If-then (no else)
pretty_write_body((Cond -> Then), OutputStream, IndentLevel) :-
    !,
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '('),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    pretty_write_goal(Cond, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '->'),
    nl(OutputStream),
    pretty_write_body(Then, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ')').

% Disjunction (or)
pretty_write_body((Goal ; Rest), OutputStream, IndentLevel) :-
    !,
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '('),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    pretty_write_body(Goal, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ';'),
    nl(OutputStream),
    pretty_write_body(Rest, OutputStream, NextLevel),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ')').

% Conjunction (and)
pretty_write_body((Goal, Rest), OutputStream, IndentLevel) :-
    !,
    write_indent(OutputStream, IndentLevel),
    pretty_write_goal(Goal, OutputStream, IndentLevel),
    write(OutputStream, ','),
    nl(OutputStream),
    pretty_write_body(Rest, OutputStream, IndentLevel).

% Negation
pretty_write_body(\+ Goal, OutputStream, IndentLevel) :-
    !,
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, '\\+ '),
    (is_simple_negated_goal(Goal) ->
        % Simple goal - write on same line
        pretty_write_goal(Goal, OutputStream, IndentLevel)
    ;
        % Complex goal - write on next line with indent
        nl(OutputStream),
        NextLevel is IndentLevel + 1,
        pretty_write_body(Goal, OutputStream, NextLevel)
    ).

% Single goal
pretty_write_body(Goal, OutputStream, IndentLevel) :-
    write_indent(OutputStream, IndentLevel),
    pretty_write_goal(Goal, OutputStream, IndentLevel).

% is_simple_negated_goal(+Goal)
% Check if a negated goal is simple enough to stay on same line.
is_simple_negated_goal(Goal) :-
    \+ (Goal = (_ , _)),
    \+ (Goal = (_ ; _)),
    \+ (Goal = (_ -> _)),
    \+ (Goal = (\+ _)),
    \+ (Goal = findall(_, _, _)).

% pretty_write_goal(+Goal, +OutputStream, +IndentLevel)
% Write a single goal with context-aware formatting.

% is_complex_control_goal(+Goal)
% Check if a goal is a complex control structure that needs indentation.
is_complex_control_goal(Goal) :-
    compound(Goal),
    (Goal = (_ , _) ; Goal = (_ ; _) ; Goal = (_ -> _) ; Goal = (\+ _)).

% findall with indentation
pretty_write_goal(findall(Template, Goal, Result), OutputStream, IndentLevel) :-
    !,
    write(OutputStream, 'findall('),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    write_term(OutputStream, Template, [numbervars(true), quoted(true)]),
    write(OutputStream, ','),
    nl(OutputStream),
    write_indent(OutputStream, NextLevel),
    % Format the goal - check if it needs indentation
    (is_complex_control_goal(Goal) ->
        % Complex goal - use body formatter with parens
        write(OutputStream, '('),
        nl(OutputStream),
        NextNextLevel is NextLevel + 1,
        pretty_write_body(Goal, OutputStream, NextNextLevel),
        nl(OutputStream),
        write_indent(OutputStream, NextLevel),
        write(OutputStream, ')')
    ;
        % Simple goal - write inline
        write_term(OutputStream, Goal, [numbervars(true), quoted(true)])
    ),
    write(OutputStream, ','),
    nl(OutputStream),
    write_indent(OutputStream, NextLevel),
    write_term(OutputStream, Result, [numbervars(true), quoted(true)]),
    nl(OutputStream),
    write_indent(OutputStream, IndentLevel),
    write(OutputStream, ')').

% Handle 'is' expressions with findall (2 args in Starlog form)
pretty_write_goal(Out is findall(Template, Goal), OutputStream, IndentLevel) :-
    !,
    write_term(OutputStream, Out, [numbervars(true), quoted(true)]),
    write(OutputStream, ' is '),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    write(OutputStream, 'findall('),
    nl(OutputStream),
    NextNextLevel is NextLevel + 1,
    write_indent(OutputStream, NextNextLevel),
    write_term(OutputStream, Template, [numbervars(true), quoted(true)]),
    write(OutputStream, ','),
    nl(OutputStream),
    write_indent(OutputStream, NextNextLevel),
    % Check if goal is complex - if so, wrap in parens and indent
    (is_complex_control_goal(Goal) ->
        % Complex goal - format with indentation and wrap in parens
        write(OutputStream, '('),
        nl(OutputStream),
        NextNextNextLevel is NextNextLevel + 1,
        pretty_write_body(Goal, OutputStream, NextNextNextLevel),
        nl(OutputStream),
        write_indent(OutputStream, NextNextLevel),
        write(OutputStream, ')')
    ;
        % Simple goal - write inline
        write_term(OutputStream, Goal, [numbervars(true), quoted(true)])
    ),
    nl(OutputStream),
    write_indent(OutputStream, NextLevel),
    write(OutputStream, ')').

% Handle 'is' expressions with find (2 args in Starlog form)
pretty_write_goal(Out is find(Template, Goal), OutputStream, IndentLevel) :-
    !,
    write_term(OutputStream, Out, [numbervars(true), quoted(true)]),
    write(OutputStream, ' is '),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    write(OutputStream, 'find('),
    nl(OutputStream),
    NextNextLevel is NextLevel + 1,
    write_indent(OutputStream, NextNextLevel),
    write_term(OutputStream, Template, [numbervars(true), quoted(true)]),
    write(OutputStream, ','),
    nl(OutputStream),
    write_indent(OutputStream, NextNextLevel),
    % Check if goal is complex - if so, wrap in parens and indent
    (is_complex_control_goal(Goal) ->
        % Complex goal - format with indentation and wrap in parens
        write(OutputStream, '('),
        nl(OutputStream),
        NextNextNextLevel is NextNextLevel + 1,
        pretty_write_body(Goal, OutputStream, NextNextNextLevel),
        nl(OutputStream),
        write_indent(OutputStream, NextNextLevel),
        write(OutputStream, ')')
    ;
        % Simple goal - write inline
        write_term(OutputStream, Goal, [numbervars(true), quoted(true)])
    ),
    nl(OutputStream),
    write_indent(OutputStream, NextLevel),
    write(OutputStream, ')').

% Handle 'is' expressions that might have complex RHS (but not findall)
pretty_write_goal(Out is Expr, OutputStream, IndentLevel) :-
    \+ is_simple_expr(Expr),
    !,
    write_term(OutputStream, Out, [numbervars(true), quoted(true)]),
    write(OutputStream, ' is '),
    nl(OutputStream),
    NextLevel is IndentLevel + 1,
    write_indent(OutputStream, NextLevel),
    write_term(OutputStream, Expr, [numbervars(true), quoted(true)]).

% Handle control structures in goal position
pretty_write_goal(Goal, OutputStream, IndentLevel) :-
    (Goal = (_ -> _ ; _) ; Goal = (_ -> _) ; Goal = (_ ; _) ; Goal = (\+ _) ; Goal = (_ , _)),
    !,
    pretty_write_body(Goal, OutputStream, IndentLevel).

% Default: simple goal
pretty_write_goal(Goal, OutputStream, _IndentLevel) :-
    write_term(OutputStream, Goal, [numbervars(true), quoted(true)]).

% is_simple_expr(+Expr)
% Check if an expression is simple enough not to need indentation.
is_simple_expr(Expr) :-
    \+ compound(Expr), !.
is_simple_expr(Expr) :-
    compound(Expr),
    Expr =.. [_|Args],
    \+ (member(Arg, Args), compound(Arg), is_control_like(Arg)).

% is_control_like(+Term)
% Check if a term is a control structure that needs indentation.
is_control_like((_ , _)).
is_control_like((_ ; _)).
is_control_like((_ -> _)).
is_control_like(\+ _).
is_control_like(findall(_, _, _)).

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
    starlog_to_prolog_code(StarlogGoal, _, [print(true)]).

% starlog_to_prolog_code(+StarlogGoal, -PrologCode)
% Convert a Starlog goal to Prolog code and return it as a term.
% Does not print to stdout - only returns the code in the variable.
starlog_to_prolog_code(StarlogGoal, PrologCode) :-
    starlog_to_prolog_code(StarlogGoal, PrologCode, [print(false)]).

% starlog_to_prolog_code(+StarlogGoal, -PrologCode, +Options)
% Convert a Starlog goal to Prolog code with options.
% Options:
%   decompress(true) - Apply maximal decompression (default)
%   decompress(false) - Minimal decompression
%   print(true) - Print to stdout (default for /1 version)
%   print(false) - Do not print to stdout (default for /2 version)
starlog_to_prolog_code(StarlogGoal, PrologCode, Options) :-
    % Expand Starlog to Prolog goals
    % Decompression is automatically done by expand_starlog_goal
    % which flattens nested expressions
    starlog_expand:expand_starlog_goal(StarlogGoal, ExpandedGoal),
    % Apply human-friendly variable renaming
    rename_variables(ExpandedGoal, RenamedGoal),
    PrologCode = RenamedGoal,
    % Print to stdout only if print(true) option is present
    (member(print(true), Options) ->
        pretty_write_body(PrologCode, user_output, 0), nl
    ;
        true
    ).

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
%   output_style(nested_calls) - Apply nested call style transformation before conversion
%   output_style(method_chaining) - Apply method chaining style transformation before conversion
%   Note: output_style affects how Starlog code is represented before being converted to Prolog
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
    % Decompression is already done by expand_starlog_goal
    starlog_expand:expand_starlog_goal(Body, ExpandedBody),
    % Apply human-friendly variable renaming
    rename_variables((Head :- ExpandedBody), RenamedClause),
    RenamedClause = (RenamedHead :- RenamedBody),
    pretty_write_term_at_level((RenamedHead :- RenamedBody), OutputStream, 0),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

output_clause_as_prolog(Fact, OutputStream, _Options) :-
    % Facts are output as-is
    rename_variables(Fact, RenamedFact),
    write_term(OutputStream, RenamedFact, [numbervars(true), quoted(true)]),
    write(OutputStream, '.'), nl(OutputStream), nl(OutputStream).

% ============================================================
% NeuroProlog PR2 helpers (Stage 2 core + Stage 1 compatibility surface)
% ============================================================

% npl_detect_polynomial_degree(+Samples, -Degree)
% Detect a plausible polynomial degree for Samples (X-Y pairs).
npl_detect_polynomial_degree(Samples, Degree) :-
    npl_detect_polynomial_degree(Samples, _Var, Degree).

% npl_detect_polynomial_degree(+Samples, +InfoVar, -Degree)
% Stage 3 compatibility arity. InfoVar is currently informational.
npl_detect_polynomial_degree(Samples, _InfoVar, Degree) :-
    samples_xy(Samples, Xs, Ys),
    length(Samples, Len),
    Len >= 2,
    max_candidate_degree(Len, MaxDegree),
    between(1, MaxDegree, Degree),
    npl_solve_polynomial_coeffs(Samples, Degree, Coeffs),
    coefficients_match_samples(Xs, Ys, Coeffs),
    !.

max_candidate_degree(Len, MaxDegree) :-
    % For short datasets (<=4 points), allow Len-1 so quadratic/cubic
    % sequences can still be detected from minimal samples.
    % For larger datasets, use Len-2 to avoid always fitting by interpolation.
    (Len =< 4 ->
        MaxDegree is Len - 1
    ;
        MaxDegree is Len - 2
    ).

% npl_build_polynomial_system(+Samples, +Degree, -Matrix, -Vector)
% Build Vandermonde-like system:
% a0*X^0 + a1*X^1 + ... + aK*X^K = Y.
npl_build_polynomial_system(Samples, Degree, Matrix, Vector) :-
    integer(Degree),
    Degree >= 0,
    findall(Row-Value,
        (member(X-Y, Samples),
         polynomial_basis_row(X, Degree, Row),
         Value = Y),
        RowValuePairs),
    pairs_to_matrix_vector(RowValuePairs, Matrix, Vector).

npl_solve_polynomial_coeffs(Samples, Degree, Coeffs) :-
    SolveCount is Degree + 1,
    take_n_samples(SolveCount, Samples, SolveSamples),
    npl_build_polynomial_system(SolveSamples, Degree, Matrix, Vector),
    npl_gaussian_elimination(Matrix, Vector, Coeffs).

take_n_samples(N, Samples, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, Samples).

pairs_to_matrix_vector([], [], []).
pairs_to_matrix_vector([Row-Value|Rest], [Row|Rows], [Value|Values]) :-
    pairs_to_matrix_vector(Rest, Rows, Values).

polynomial_basis_row(X, Degree, Row) :-
    findall(PowerValue,
        (between(0, Degree, P),
         pow_number(X, P, PowerValue)),
        Row).

pow_number(_, 0, 1) :- !.
pow_number(X, P, Value) :-
    P > 0,
    Value is X^P.

% npl_gaussian_elimination(+Matrix, +Vector, -Coefficients)
% Solve linear system Matrix * Coefficients = Vector using Gaussian elimination.
npl_gaussian_elimination(Matrix, Vector, Coefficients) :-
    augment_matrix(Matrix, Vector, Augmented),
    once(gaussian_elimination:solve_system(Augmented, RawCoefficients, unique)),
    normalize_coefficients(RawCoefficients, Coefficients).

augment_matrix([], [], []).
augment_matrix([Row|Rows], [Value|Values], [AugRow|AugRows]) :-
    append(Row, [Value], AugRow),
    augment_matrix(Rows, Values, AugRows).

normalize_coefficients([], []).
normalize_coefficients([C|Cs], [N|Ns]) :-
    normalize_number(C, N),
    normalize_coefficients(Cs, Ns).

normalize_number(Value, Normalized) :-
    V is Value,
    npl_epsilon(Eps),
    (abs(V) < Eps ->
        Normalized = 0
    ;
        Rounded is round(V),
        (abs(V - Rounded) < Eps ->
            Normalized = Rounded
        ;
            Normalized = V
        )
    ).

npl_epsilon(1.0e-12).
% Use a small fixed sampling window for rewrite discovery to keep calls bounded.
% Six points are enough to reject common non-polynomial recurrences (e.g. Fibonacci)
% while still cheaply identifying practical low-degree polynomial fits.
npl_max_sample_count(6).

% npl_reconstruct_polynomial(+Var, +Coefficients, -Expr)
% Build symbolic expression from coefficients [a0, a1, ...].
npl_reconstruct_polynomial(Var, Coefficients, Expr) :-
    coefficient_terms(Var, Coefficients, 0, Terms),
    (Terms = [] ->
        Expr = 0
    ;
        terms_sum(Terms, Expr)
    ).

coefficient_terms(_, [], _, []).
coefficient_terms(Var, [Coeff|Coeffs], Power, Terms) :-
    (Coeff =:= 0 ->
        Terms = RestTerms
    ;
        polynomial_term(Var, Coeff, Power, Term),
        Terms = [Term|RestTerms]
    ),
    NextPower is Power + 1,
    coefficient_terms(Var, Coeffs, NextPower, RestTerms).

polynomial_term(_, Coeff, 0, Coeff) :- !.
polynomial_term(Var, Coeff, 1, Term) :-
    !,
    multiply_simplified(Coeff, Var, Term).
polynomial_term(Var, Coeff, Power, Term) :-
    Power > 1,
    multiply_simplified(Coeff, Var^Power, Term).

multiply_simplified(1, Expr, Expr) :- !.
multiply_simplified(-1, Expr, -Expr) :- !.
multiply_simplified(Coeff, Expr, Coeff*Expr).

terms_sum([Term], Term) :- !.
terms_sum([Head|Tail], Expr) :-
    terms_sum(Tail, RestExpr),
    Expr = Head + RestExpr.

% npl_validate_polynomial_formula(+Samples, +Expr, +Var, -Result)
% Validate formula against samples; derives Expr when unbound.
npl_validate_polynomial_formula(Samples, Expr, Var, Result) :-
    (npl_fit_polynomial(Samples, Var, _Coeffs, DerivedExpr) ->
        (var(Expr) -> Expr = DerivedExpr ; Expr =@= DerivedExpr),
        Result = accepted
    ;
        Result = rejected_non_polynomial
    ).

npl_fit_polynomial(Samples, Var, Coeffs, Expr) :-
    npl_discover_polynomial_fit(Samples, Var, Coeffs, Expr, accepted).

npl_discover_polynomial_fit(Samples, Var, Coeffs, Expr, Result) :-
    npl_detect_polynomial_degree(Samples, Var, Degree),
    npl_solve_polynomial_coeffs(Samples, Degree, Coeffs),
    npl_validate_polynomial_fit(Samples, Coeffs, Var, FitResult),
    (FitResult == accepted ->
        npl_reconstruct_polynomial(Var, Coeffs, Expr),
        Result = accepted
    ;
        Result = rejected_non_polynomial
    ).

samples_xy([], [], []).
samples_xy([X-Y|Rest], [X|Xs], [Y|Ys]) :-
    samples_xy(Rest, Xs, Ys).

coefficients_match_samples([], [], _).
coefficients_match_samples([X|Xs], [Y|Ys], Coeffs) :-
    evaluate_polynomial_coeffs(Coeffs, X, Value),
    normalize_number(Value, NValue),
    normalize_number(Y, NY),
    NValue =:= NY,
    coefficients_match_samples(Xs, Ys, Coeffs).

evaluate_polynomial_coeffs(Coeffs, X, Value) :-
    evaluate_polynomial_coeffs(Coeffs, X, 0, 0, Value).

evaluate_polynomial_coeffs([], _, _, Acc, Acc).
evaluate_polynomial_coeffs([Coeff|Rest], X, Power, Acc, Value) :-
    pow_number(X, Power, XPow),
    Acc1 is Acc + Coeff * XPow,
    NextPower is Power + 1,
    evaluate_polynomial_coeffs(Rest, X, NextPower, Acc1, Value).

% npl_extract_numeric_samples(+Goal, +Var, +MaxSamples, -Samples)
% Extract X-Y sample points for polynomial fitting from safe numeric goals.
npl_extract_numeric_samples(samples(Samples), _Var, _MaxSamples, Samples) :-
    !,
    npl_samples_are_numeric_pairs(Samples).
npl_extract_numeric_samples(numeric_samples(Samples), _Var, _MaxSamples, Samples) :-
    !,
    npl_samples_are_numeric_pairs(Samples).
npl_extract_numeric_samples(sequence(Values), _Var, _MaxSamples, Samples) :-
    !,
    is_list(Values),
    npl_sequence_to_samples(Values, 1, Samples),
    npl_samples_are_numeric_pairs(Samples).
npl_extract_numeric_samples(Goal, Var, MaxSamples, Samples) :-
    integer(MaxSamples),
    MaxSamples >= 2,
    callable(Goal),
    term_variables(Goal, Vars),
    memberchk(Var, Vars),
    select(Var, Vars, RemainingVars),
    RemainingVars = [OutputVar],
    npl_collect_goal_samples(1, MaxSamples, Goal, Var, OutputVar, Samples),
    length(Samples, Count),
    Count >= 2.

npl_samples_are_numeric_pairs([]).
npl_samples_are_numeric_pairs([X-Y|Rest]) :-
    number(X),
    number(Y),
    npl_samples_are_numeric_pairs(Rest).

npl_sequence_to_samples([], _Index, []).
npl_sequence_to_samples([Y|Ys], Index, [Index-Y|Rest]) :-
    number(Y),
    NextIndex is Index + 1,
    npl_sequence_to_samples(Ys, NextIndex, Rest).

npl_collect_goal_samples(Index, Max, _Goal, _Var, _OutputVar, []) :-
    Index > Max,
    !.
npl_collect_goal_samples(Index, Max, Goal, Var, OutputVar, [Index-Y|Rest]) :-
    copy_term(Goal-Var-OutputVar, GoalI-VarI-OutputI),
    VarI = Index,
    once(call(GoalI)),
    number(OutputI),
    !,
    Y = OutputI,
    Next is Index + 1,
    npl_collect_goal_samples(Next, Max, Goal, Var, OutputVar, Rest).
npl_collect_goal_samples(_, _, _, _, _, []).

% npl_validate_polynomial_fit(+Samples, +Coefficients, +Var, -Result)
% Validate that solved coefficients fit all samples.
npl_validate_polynomial_fit(Samples, Coefficients, _Var, Result) :-
    samples_xy(Samples, Xs, Ys),
    (coefficients_match_samples(Xs, Ys, Coefficients) ->
        Result = accepted
    ;
        Result = rejected_non_polynomial
    ).

% npl_rewrite_recurrence_to_closed_form(+Goal, +Var, -Expr, -Result)
npl_rewrite_recurrence_to_closed_form(Goal, _Var, _Expr, rejected_impure) :-
    \+ npl_pure_goal(Goal),
    !.
npl_rewrite_recurrence_to_closed_form(Goal, Var, Expr, accepted) :-
    npl_max_sample_count(MaxSamples),
    npl_extract_numeric_samples(Goal, Var, MaxSamples, Samples),
    npl_discover_polynomial_fit(Samples, Var, _Coeffs, Expr, DiscoverResult),
    DiscoverResult == accepted,
    !.
npl_rewrite_recurrence_to_closed_form(_Goal, _Var, _Expr, rejected_non_polynomial).

npl_pure_goal((A, B)) :-
    !,
    npl_pure_goal(A),
    npl_pure_goal(B).
npl_pure_goal((A ; B)) :-
    !,
    npl_pure_goal(A),
    npl_pure_goal(B).
npl_pure_goal(\+ A) :-
    !,
    npl_pure_goal(A).
npl_pure_goal(Goal) :-
    callable(Goal),
    Goal =.. [Name|_],
    \+ npl_impure_predicate(Name).

npl_impure_predicate(write).
npl_impure_predicate(writeln).
npl_impure_predicate(print).
npl_impure_predicate(format).
npl_impure_predicate(read).
npl_impure_predicate(open).
npl_impure_predicate(close).
npl_impure_predicate(assert).
npl_impure_predicate(asserta).
npl_impure_predicate(assertz).
npl_impure_predicate(retract).
npl_impure_predicate(retractall).
npl_impure_predicate(abolish).
npl_impure_predicate(halt).
npl_impure_predicate(shell).
npl_impure_predicate(sleep).

% Stage 4 indexed tracing helpers

npl_assign_symbolic_indices(Structure, indexed(Structure), map(Map)) :-
    npl_collect_symbolic_indices(Structure, [], RawMap),
    sort(RawMap, Map).

npl_collect_symbolic_indices(Var, Path, [path(Path)-Var]) :-
    var(Var),
    !.
npl_collect_symbolic_indices([], _Path, []) :-
    !.
npl_collect_symbolic_indices(List, ParentPath, Map) :-
    is_list(List),
    !,
    npl_collect_list_symbolic_indices(List, 1, ParentPath, Map).
npl_collect_symbolic_indices(Term, ParentPath, [path(ParentPath)-Term|Map]) :-
    compound(Term),
    !,
    Term =.. [_Name|Args],
    npl_collect_list_symbolic_indices(Args, 1, ParentPath, Map).
npl_collect_symbolic_indices(Value, Path, [path(Path)-Value]).

npl_collect_list_symbolic_indices([], _Index, _ParentPath, []).
npl_collect_list_symbolic_indices([Item|Rest], Index, ParentPath, Map) :-
    append(ParentPath, [Index], Path),
    npl_collect_symbolic_indices(Item, Path, ItemMap),
    NextIndex is Index + 1,
    npl_collect_list_symbolic_indices(Rest, NextIndex, ParentPath, RestMap),
    append(ItemMap, RestMap, Map).

npl_trace_index_flow(Goal, IndexMap, flow_graph(Reduced, IndexMap, Pairs, trace(Meta))) :-
    npl_reduce_predicate_to_pattern_irreducibles(Goal, Reduced),
    npl_extract_flow_pairs(Goal, Pairs),
    length(Pairs, Count),
    npl_general_trace_metadata(Count, Meta).

npl_general_trace_metadata(Count, [pair_count(Count), optimization_class(general_first_principles), named_special_cases(none)]).

npl_extract_flow_pairs(flow(Pairs), Pairs) :-
    is_list(Pairs),
    !.
npl_extract_flow_pairs(flow_graph(_, _, Pairs, _), Pairs) :-
    is_list(Pairs),
    !.
npl_extract_flow_pairs(goal_pairs(Pairs), Pairs) :-
    is_list(Pairs),
    !.
npl_extract_flow_pairs(_, []).

npl_identify_independent_indices(flow_graph(_, _, Pairs, _), [i]) :-
    npl_pairs_have_numeric_indices(Pairs),
    !.
npl_identify_independent_indices(flow(Pairs), [i]) :-
    npl_pairs_have_numeric_indices(Pairs),
    !.
npl_identify_independent_indices(_, []).

npl_pairs_have_numeric_indices(Pairs) :-
    is_list(Pairs),
    member(_Name-Samples, Pairs),
    is_list(Samples),
    member(X-_, Samples),
    number(X),
    !.

npl_emit_direct_indexed_rule(FlowGraph, IndependentVars, Relations, direct_rule(FlowGraph, IndependentVars, Rule)) :-
    npl_reconstruct_direct_indexed_rule(Relations, IndependentVars, Rule).

npl_reconstruct_direct_indexed_rule(Relations, Coefficients, direct_index_rule(Relations, coefficient_metadata(Coefficients))) :-
    is_list(Relations),
    npl_numeric_coefficients_list(Coefficients),
    !.
npl_reconstruct_direct_indexed_rule(Relations, SecondArg, direct_index_rule(Relations)) :-
    is_list(Relations),
    \+ npl_numeric_coefficients_list(SecondArg).

npl_numeric_coefficients_list(Coefficients) :-
    is_list(Coefficients),
    Coefficients \== [],
    forall(member(Coeff, Coefficients), number(Coeff)).

npl_reconstruct_index_relations(flow_graph(_, _, Pairs, _), IndependentVars, Relations) :-
    !,
    npl_reconstruct_index_relations(flow(Pairs), IndependentVars, Relations).
npl_reconstruct_index_relations(flow(Pairs), [], Relations) :-
    !,
    npl_reconstruct_index_relations(flow(Pairs), [i], Relations).
npl_reconstruct_index_relations(flow(Pairs), [IdxVar|_], Relations) :-
    is_list(Pairs),
    !,
    findall(Name-Expr,
        ( member(Name-Samples, Pairs),
          relation_from_samples(Samples, IdxVar, Expr)
        ),
        Relations).
npl_reconstruct_index_relations(_, _, []).

relation_from_samples(Samples, IdxVar, Expr) :-
    npl_detect_polynomial_degree(Samples, Degree),
    npl_build_polynomial_system(Samples, Degree, Matrix, Vector),
    npl_gaussian_elimination(Matrix, Vector, Coeffs),
    npl_reconstruct_polynomial(IdxVar, Coeffs, RawExpr),
    npl_normalize_relation_expression(Samples, IdxVar, Coeffs, RawExpr, Expr).

npl_normalize_relation_expression([FirstX-FirstY|Rest], IdxVar, [_Offset, 1], _RawExpr, Expr) :-
    unit_step_index_sequence([FirstX-FirstY|Rest]),
    !,
    Expr = (FirstY + IdxVar - FirstX).
npl_normalize_relation_expression(_Samples, IdxVar, Coeffs, _RawExpr, Expr) :-
    npl_reconstruct_polynomial_desc(IdxVar, Coeffs, Expr).

unit_step_index_sequence([_]).
unit_step_index_sequence([X1-Y1, X2-Y2|Rest]) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    DX =:= 1,
    DY =:= 1,
    unit_step_index_sequence([X2-Y2|Rest]).

npl_reconstruct_polynomial_desc(Var, Coeffs, Expr) :-
    length(Coeffs, Len),
    MaxPower is Len - 1,
    findall(Term,
        ( between(0, MaxPower, InvP),
          Power is MaxPower - InvP,
          nth0(Power, Coeffs, Coeff),
          Coeff =\= 0,
          polynomial_term(Var, Coeff, Power, Term)
        ),
        Terms),
    ( Terms = [] ->
        Expr = 0
    ;
        terms_sum(Terms, Expr)
    ).

% npl_reduce_predicate_to_pattern_irreducibles(+Goal, -Reduced)
npl_reduce_predicate_to_pattern_irreducibles(non_reducible_external_call(_), non_reducible) :-
    !.
npl_reduce_predicate_to_pattern_irreducibles((A, B), reduced(sequence(RA, RB))) :-
    !,
    npl_reduce_predicate_to_pattern_irreducibles(A, RA),
    npl_reduce_predicate_to_pattern_irreducibles(B, RB).
npl_reduce_predicate_to_pattern_irreducibles((A ; B), reduced(choice(RA, RB))) :-
    !,
    npl_reduce_predicate_to_pattern_irreducibles(A, RA),
    npl_reduce_predicate_to_pattern_irreducibles(B, RB).
npl_reduce_predicate_to_pattern_irreducibles(\+ A, reduced(negation(RA))) :-
    !,
    npl_reduce_predicate_to_pattern_irreducibles(A, RA).
npl_reduce_predicate_to_pattern_irreducibles(Goal, reduced(Classified)) :-
    callable(Goal),
    Goal =.. [Name|Args],
    length(Args, Arity),
    Functor = Name/Arity,
    ( npl_pattern_matching_predicate(Functor) ->
        Kind = pattern_matching
    ; npl_irreducible_predicate(Functor) ->
        Kind = irreducible
    ;
        Kind = reducible_custom
    ),
    Classified = node(Kind, Functor, Goal).

npl_pattern_matching_predicate(member/2).
npl_pattern_matching_predicate(nth1/3).
npl_pattern_matching_predicate(arg/3).
npl_pattern_matching_predicate(append/3).
npl_pattern_matching_predicate(sub_string/5).
npl_pattern_matching_predicate((=..)/2).
npl_pattern_matching_predicate((=)/2).

npl_irreducible_predicate((is)/2).
npl_irreducible_predicate((>)/2).
npl_irreducible_predicate((<)/2).
npl_irreducible_predicate((>=)/2).
npl_irreducible_predicate((=<)/2).
npl_irreducible_predicate((=:=)/2).
npl_irreducible_predicate((=\=)/2).

npl_collect_formula_samples(flow_graph(_, _, Pairs, _), _Var, Pairs) :-
    is_list(Pairs),
    !.
npl_collect_formula_samples(flow(Pairs), _Var, Pairs) :-
    is_list(Pairs),
    !.
npl_collect_formula_samples(Relations, IndependentVar, SamplesByRelation) :-
    is_list(Relations),
    npl_max_sample_count(MaxSamples),
    findall(Name-Samples,
        ( member(Name-RelationExpr, Relations),
          nonvar(RelationExpr),
          npl_relation_expr_samples(RelationExpr, IndependentVar, 1, MaxSamples, Samples)
        ),
        SamplesByRelation),
    !.
npl_collect_formula_samples(_, _, []).

npl_relation_expr_samples(_Expr, _IndexAtom, Index, MaxSamples, []) :-
    Index > MaxSamples,
    !.
npl_relation_expr_samples(Expr, IndexAtom, Index, MaxSamples, [Index-Value|Rest]) :-
    npl_eval_relation_expr_with_index(Expr, IndexAtom, Index, Value),
    NextIndex is Index + 1,
    npl_relation_expr_samples(Expr, IndexAtom, NextIndex, MaxSamples, Rest),
    !.
npl_relation_expr_samples(_, _, _, _, []).

npl_validate_direct_rule(OriginalGoal, DirectRule, Result) :-
    npl_extract_flow_pairs(OriginalGoal, Pairs),
    npl_direct_rule_relations(DirectRule, Relations),
    ( npl_relations_match_samples(Relations, Pairs) ->
        Result = accepted
    ;
        Result = rejected_non_equivalent
    ).

npl_direct_rule_relations(direct_rule(_, _, direct_index_rule(Relations)), Relations) :-
    !.
npl_direct_rule_relations(direct_rule(_, _, direct_index_rule(Relations, _)), Relations) :-
    !.
npl_direct_rule_relations(direct_rule(_, _, Relations), Relations) :-
    is_list(Relations),
    !.
npl_direct_rule_relations(direct_index_rule(Relations, _), Relations) :-
    is_list(Relations),
    !.
npl_direct_rule_relations(direct_index_rule(Relations), Relations) :-
    is_list(Relations).

npl_relations_match_samples(Relations, Pairs) :-
    forall(
        member(Name-Samples, Pairs),
        ( member(Name-Expr, Relations),
          npl_expression_matches_samples(Expr, Samples)
        )
    ).

npl_expression_matches_samples(_Expr, []).
npl_expression_matches_samples(Expr, [X-Y|Rest]) :-
    npl_eval_relation_expr(Expr, X, Value),
    normalize_number(Value, NV),
    normalize_number(Y, NY),
    NV =:= NY,
    npl_expression_matches_samples(Expr, Rest).

npl_eval_relation_expr(Expr, IndexValue, Value) :-
    npl_eval_relation_expr_with_index(Expr, i, IndexValue, Value).

npl_eval_relation_expr_with_index(Expr, IndexAtom, IndexValue, Value) :-
    npl_substitute_index_atom(Expr, IndexAtom, IndexValue, ExprGrounded),
    copy_term(ExprGrounded, ExprCopy),
    term_variables(ExprCopy, Vars),
    maplist(=(IndexValue), Vars),
    Value is ExprCopy.

% Backward-compatible default for legacy callers; new code should prefer
% npl_substitute_index_atom/4 with an explicit IndexAtom.
npl_substitute_index_atom(Expr, X, Substituted) :-
    npl_substitute_index_atom(Expr, i, X, Substituted).

npl_substitute_index_atom(Var, _IndexAtom, _X, Var) :-
    var(Var),
    !.
npl_substitute_index_atom(IndexAtom, IndexAtom, X, X) :-
    atom(IndexAtom),
    !.
npl_substitute_index_atom(Atom, _IndexAtom, _X, Atom) :-
    atomic(Atom),
    !.
npl_substitute_index_atom(Term, IndexAtom, X, Substituted) :-
    Term =.. [Name|Args],
    maplist(npl_substitute_index_atom_with(IndexAtom, X), Args, SubArgs),
    Substituted =.. [Name|SubArgs].

npl_substitute_index_atom_with(IndexAtom, X, Arg, SubArg) :-
    npl_substitute_index_atom(Arg, IndexAtom, X, SubArg).

npl_should_preserve_full_structure(full_structure_required(_), true) :-
    !.
npl_should_preserve_full_structure(goal_requires_full_structure, true) :-
    !.
npl_should_preserve_full_structure(_Goal, false).

npl_detect_ambiguous_index_mapping(FlowGraph, true) :-
    npl_extract_flow_pairs(FlowGraph, Pairs),
    member(Name-SamplesA, Pairs),
    member(Name-SamplesB, Pairs),
    SamplesA \== SamplesB,
    !.
npl_detect_ambiguous_index_mapping(_FlowGraph, false).

% ============================================================
% NeuroProlog PR2 Stage 8 helpers (IR + pipeline representation)
% ============================================================

% npl_stage8_pipeline_order(-PassOrder)
% Deterministic pass order for Stage 8 optimisation pipeline.
npl_stage8_pipeline_order([
    parse_and_analyse,
    semantic_analysis,
    recurrence_classification,
    sample_extraction,
    degree_estimation,
    gaussian_elimination_polynomial_solve,
    polynomial_validation_and_rewrite,
    symbolic_index_assignment,
    predicate_reduction_pattern_irreducibles,
    indexed_variable_flow_tracing,
    independent_variable_identification,
    formula_reconstruction_from_first_principles,
    gaussian_elimination_indexed_polynomial_formula_solve,
    direct_index_rule_reconstruction,
    simplification,
    code_generation
]).

% npl_stage8_build_ir(+FlowGraph, +IndependentVars, +Relations, +Coefficients, +Options, -IR)
% Build explicit Stage 8 IR containing relation, polynomial, direct-rule and provenance nodes.
npl_stage8_build_ir(_FlowGraph, IndependentVars, Relations, Coefficients, Options, ir_pipeline(PassOrder, IRNodes, meta(Metadata))) :-
    npl_stage8_pipeline_order(PassOrder),
    sort(IndependentVars, SortedIndependentVars),
    sort(Relations, SortedRelations),
    npl_stage8_coefficients_representation(Coefficients, Options, IRPolyCoefficients, CoeffRepresentation),
    npl_stage8_primary_index_var(SortedIndependentVars, IndexVar),
    npl_stage8_poly_eval_node(IndexVar, IRPolyCoefficients, PolyNode),
    IndexNode = ir_index_relation(SortedIndependentVars, SortedRelations),
    RuleNode = ir_direct_index_rule(index_spec(SortedIndependentVars), relations(SortedRelations), result_collector(values)),
    npl_stage8_core_nodes(PolyNode, IndexNode, RuleNode, CoreNodes),
    npl_stage8_provenance_note(Options, ProvenanceNote),
    npl_stage8_wrap_with_provenance(CoreNodes, ProvenanceNote, IRNodes),
    length(SortedRelations, RelationCount),
    Metadata = [index_relation_metadata(relation_count(RelationCount), independent_indices(SortedIndependentVars)),
                coefficient_representation(CoeffRepresentation),
                provenance(ProvenanceNote)].

npl_stage8_primary_index_var([IndexVar|_], IndexVar) :-
    !.
npl_stage8_primary_index_var([], i).

npl_stage8_poly_eval_node(IndexVar, Coefficients, ir_poly_eval(IndexVar, Coefficients, poly_result)) :-
    Coefficients \== [],
    !.
npl_stage8_poly_eval_node(_IndexVar, _Coefficients, no_poly_eval).

npl_stage8_core_nodes(no_poly_eval, IndexNode, RuleNode, [IndexNode, RuleNode]) :-
    !.
npl_stage8_core_nodes(PolyNode, IndexNode, RuleNode, [IndexNode, PolyNode, RuleNode]).

npl_stage8_provenance_note(Options, Note) :-
    (memberchk(provenance(Note), Options) ->
        true
    ;
        Note = stage8_first_principles_derivation
    ).

npl_stage8_coefficients_representation(Coefficients, Options, Representation, rational) :-
    memberchk(rational_coefficients(true), Options),
    !,
    maplist(npl_stage8_to_rational_term, Coefficients, Representation).
npl_stage8_coefficients_representation(Coefficients, _Options, Coefficients, native).

npl_stage8_to_rational_term(Coefficient, rational(Coefficient)).

npl_stage8_wrap_with_provenance([], _Note, []).
npl_stage8_wrap_with_provenance([Node|Nodes], Note, [ir_provenance(Note, Node)|Wrapped]) :-
    npl_stage8_wrap_with_provenance(Nodes, Note, Wrapped).

% npl_stage8_ir_provenance(+IR, -Provenance)
% Extract inspectable provenance notes from Stage 8 IR.
npl_stage8_ir_provenance(ir_pipeline(_, Nodes, meta(Metadata)), Provenance) :-
    npl_stage8_collect_node_provenance(Nodes, NodeNotes),
    (member(provenance(MetaNote), Metadata) ->
        append(NodeNotes, [MetaNote], Notes)
    ;
        Notes = NodeNotes
    ),
    sort(Notes, Provenance).

npl_stage8_collect_node_provenance([], []).
npl_stage8_collect_node_provenance([ir_provenance(Note, _)|Nodes], [Note|Notes]) :-
    !,
    npl_stage8_collect_node_provenance(Nodes, Notes).
npl_stage8_collect_node_provenance([_|Nodes], Notes) :-
    npl_stage8_collect_node_provenance(Nodes, Notes).

% npl_stage8_lower_ir(+IR, -LoweredIR)
% Lower Stage 8 IR nodes to executable-oriented intermediate form.
npl_stage8_lower_ir(ir_pipeline(_PassOrder, Nodes, _Meta), lowered_ir(LoweredNodes)) :-
    maplist(npl_stage8_lower_ir_node, Nodes, LoweredNodes).

npl_stage8_lower_ir_node(ir_provenance(_Note, Node), LoweredNode) :-
    !,
    npl_stage8_lower_ir_node(Node, LoweredNode).
npl_stage8_lower_ir_node(ir_poly_eval(IndexVar, Coefficients, ResultVar),
                         lowered_poly_eval(IndexVar, Coefficients, ResultVar)) :-
    !.
npl_stage8_lower_ir_node(ir_index_relation(IndependentVars, Relations),
                         lowered_index_relation(IndependentVars, Relations)) :-
    !.
npl_stage8_lower_ir_node(ir_direct_index_rule(IndexSpec, relations(Relations), ResultCollector),
                         direct_index_rule(IndexSpec, Relations, ResultCollector)) :-
    !.
npl_stage8_lower_ir_node(Node, lowered_passthrough(Node)).
