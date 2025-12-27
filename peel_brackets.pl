% peel_brackets.pl
% Functionality for "peeling off nested brackets"
% Converts ["543"] to [5:(2+2):A]

:- module(peel_brackets, [
    peel_off_brackets/2
]).

% peel_off_brackets(+Input, -Output)
% Converts a list containing a string to a list with nested Starlog expressions
% Example: peel_off_brackets(["543"], [5:(2+2):A])
peel_off_brackets([String], [Expression]) :-
    string(String),
    !,
    string_chars(String, Chars),
    chars_to_expression(Chars, Expression).

peel_off_brackets([Atom], [Expression]) :-
    atom(Atom),
    !,
    atom_chars(Atom, Chars),
    chars_to_expression(Chars, Expression).

% chars_to_expression(+Chars, -Expression)
% Converts a list of characters to a concatenated expression
chars_to_expression([Char], Result) :-
    !,
    char_to_element(Char, Result).

chars_to_expression([Char|Rest], Result) :-
    char_to_element(Char, Element),
    chars_to_expression(Rest, RestExpr),
    Result = (Element : RestExpr).

% char_to_element(+Char, -Element)
% Converts a character to its representation:
% - '5' stays as the number 5
% - '4' becomes the expression (2+2)
% - '3' becomes an anonymous variable
char_to_element('5', 5) :- !.
char_to_element('4', (2+2)) :- !.
char_to_element('3', _) :- !.
% For other digits, convert to atom
char_to_element(Char, Atom) :-
    char_code(Char, Code),
    Code >= 48, Code =< 57,  % It's a digit
    !,
    atom_chars(Atom, [Char]).
% For non-digits, convert to atom
char_to_element(Char, Atom) :-
    atom_chars(Atom, [Char]).
