% sample_for_compression.pl
% Sample Prolog file to test compression

% Simple predicate with sequential operations
greet(First, Last, Greeting) :-
    string_concat("Hello, ", First, Temp),
    string_concat(Temp, " ", Temp2),
    string_concat(Temp2, Last, Greeting).

% Predicate with list operations
combine_and_reverse(A, B, Result) :-
    append(A, B, Combined),
    reverse(Combined, Result).

% Complex nested operations
process_strings(A, B, C, Result) :-
    string_concat(A, B, AB),
    string_concat(AB, C, ABC),
    string_length(ABC, Len),
    atom_concat(result, Len, Result).
