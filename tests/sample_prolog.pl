% sample_prolog.pl
% A sample Prolog file for testing starlog_output_file

% Simple string concatenation
greet(First, Last, Greeting) :-
    string_concat("Hello, ", First, Temp),
    string_concat(Temp, " ", Temp2),
    string_concat(Temp2, Last, Greeting).

% List operations
combine_lists(L1, L2, Combined, Reversed) :-
    append(L1, L2, Combined),
    reverse(Combined, Reversed).

% Mixed operations
process_data(Input, Output) :-
    string_concat(Input, "_processed", Temp),
    atom_concat(result, _, Final),
    append([Temp], [Final], Output).
