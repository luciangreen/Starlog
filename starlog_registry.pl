% starlog_registry.pl
% Registry for builtin predicate mappings in Starlog-in-Prolog
% This module maintains mappings between Starlog value-returning syntax
% and standard Prolog predicates.

:- module(starlog_registry, [
    is_value_builtin/3,
    register_value_builtin/3,
    unregister_value_builtin/2
]).

:- dynamic value_builtin_map/3.

% is_value_builtin(?Name, ?Arity, ?PrologPred)
% Checks if a functor/arity is registered as a value builtin
% and provides the corresponding Prolog predicate functor.
is_value_builtin(Name, Arity, PrologPred) :-
    value_builtin_map(Name, Arity, PrologPred), !.
is_value_builtin(Name, Arity, Name) :-
    default_value_builtin(Name, Arity).

% register_value_builtin(+Name, +Arity, +PrologPred)
% Register a new value-returning builtin mapping.
% Example: register_value_builtin(foo, 2, foo) means
%   Out is foo(A,B) expands to foo(A,B,Out)
register_value_builtin(Name, Arity, PrologPred) :-
    retractall(value_builtin_map(Name, Arity, _)),
    assertz(value_builtin_map(Name, Arity, PrologPred)).

% unregister_value_builtin(+Name, +Arity)
% Remove a custom value builtin mapping.
unregister_value_builtin(Name, Arity) :-
    retractall(value_builtin_map(Name, Arity, _)).

% Default value builtins (built-in to Starlog)
% String and atom operations
default_value_builtin(string_length, 1).
default_value_builtin(atom_length, 1).
default_value_builtin(number_string, 1).
default_value_builtin(number_to_string, 1).
default_value_builtin(string_chars, 1).
default_value_builtin(atom_chars, 1).
default_value_builtin(atom_string, 1).
default_value_builtin(atom_number, 1).
default_value_builtin(char_code, 1).
default_value_builtin(string_upper, 1).
default_value_builtin(string_lower, 1).
default_value_builtin(atom_codes, 1).
default_value_builtin(string_codes, 1).
default_value_builtin(term_string, 1).
default_value_builtin(term_to_atom, 1).
default_value_builtin(downcase_atom, 1).
default_value_builtin(upcase_atom, 1).
default_value_builtin(sub_string, 4).
default_value_builtin(sub_atom, 4).

% List operations
default_value_builtin(length, 1).
default_value_builtin(reverse, 1).
default_value_builtin(head, 1).
default_value_builtin(tail, 1).
default_value_builtin(delete, 2).
default_value_builtin(wrap, 1).
default_value_builtin(unwrap, 1).
default_value_builtin(maplist, 2).
default_value_builtin(sort, 1).
default_value_builtin(msort, 1).
default_value_builtin(keysort, 1).
default_value_builtin(intersection, 2).
default_value_builtin(union, 2).
default_value_builtin(flatten, 1).
default_value_builtin(nth0, 2).
default_value_builtin(nth1, 2).
default_value_builtin(last, 1).
default_value_builtin(min_list, 1).
default_value_builtin(max_list, 1).
default_value_builtin(sum_list, 1).
default_value_builtin(subtract, 2).
default_value_builtin(select, 2).
default_value_builtin(permutation, 1).
default_value_builtin(member, 1).

% Math operations
default_value_builtin(ceiling, 1).
default_value_builtin(floor, 1).
default_value_builtin(round, 1).
default_value_builtin(truncate, 1).
default_value_builtin(abs, 1).
default_value_builtin(sign, 1).
default_value_builtin(sqrt, 1).
default_value_builtin(sin, 1).
default_value_builtin(cos, 1).
default_value_builtin(tan, 1).
default_value_builtin(asin, 1).
default_value_builtin(acos, 1).
default_value_builtin(atan, 1).
default_value_builtin(log, 1).
default_value_builtin(log10, 1).
default_value_builtin(exp, 1).
default_value_builtin(string_to_number, 1).
default_value_builtin(random, 1).

% Other operations
default_value_builtin(findall, 2).
default_value_builtin(string_from_file, 1).
default_value_builtin(read_string, 4).
default_value_builtin(term_variables, 1).
default_value_builtin(current_output, 1).
default_value_builtin(current_input, 1).
default_value_builtin(file_base_name, 1).
default_value_builtin(file_name_extension, 2).
default_value_builtin(directory_files, 1).
default_value_builtin(working_directory, 1).
default_value_builtin(atomic_list_concat, 1).
default_value_builtin(atomic_list_concat, 2).
default_value_builtin(format_time, 3).
default_value_builtin(split_string, 3).
default_value_builtin(term_hash, 1).

% Nullary operations (no input args, just output)
default_value_builtin(date, 0).
default_value_builtin(get_time, 0).
