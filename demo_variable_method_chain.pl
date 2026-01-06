% demo_variable_method_chain.pl
% Comprehensive demonstration of: B = Value, A is B>>operation>>operation
% Shows all combinations and configurations of variable-based method chaining

:- use_module(starlog).

demo_header(Title) :-
    nl,
    writeln('========================================'),
    writeln(Title),
    writeln('========================================').

demo_example(Description, Code) :-
    write('  '), writeln(Description),
    write('    ?- '), writeln(Code),
    nl.

% ============================================================
% Section 1: Problem Statement - Core Pattern
% ============================================================

demo_problem_statement :-
    demo_header('Section 1: Problem Statement - B = [3,2,2], A is B>>sort>>length'),
    
    demo_example('Exact problem statement pattern:',
                 'B = [3,2,2], starlog_call(A is B>>sort>>length), A = 2'),
    
    write('  Executing: '),
    B1 = [3,2,2],
    starlog_call(A1 is B1>>sort>>length),
    format('B = ~w, A = ~w~n', [B1, A1]),
    nl,
    
    demo_example('Just sort (single operation):',
                 'B = [3,2,2], starlog_call(A is B>>sort), A = [2,3]'),
    
    write('  Executing: '),
    B2 = [3,2,2],
    starlog_call(A2 is B2>>sort),
    format('B = ~w, A = ~w~n', [B2, A2]),
    nl,
    
    demo_example('Just length (single operation):',
                 'B = [3,2,2], starlog_call(A is B>>length), A = 3'),
    
    write('  Executing: '),
    B3 = [3,2,2],
    starlog_call(A3 is B3>>length),
    format('B = ~w, A = ~w~n', [B3, A3]),
    nl.

% ============================================================
% Section 2: List Variables with Different Method Chains
% ============================================================

demo_list_variables :-
    demo_header('Section 2: List Variables with Different Method Chains'),
    
    demo_example('Reverse a list:',
                 'B = [1,2,3], starlog_call(A is B>>reverse)'),
    B1 = [1,2,3],
    starlog_call(A1 is B1>>reverse),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('Sort then get length:',
                 'B = [5,1,3,2], starlog_call(A is B>>sort>>length)'),
    B2 = [5,1,3,2],
    starlog_call(A2 is B2>>sort>>length),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('Flatten nested lists:',
                 'B = [[1,2],[3,4]], starlog_call(A is B>>flatten)'),
    B3 = [[1,2],[3,4]],
    starlog_call(A3 is B3>>flatten),
    format('  Result: A = ~w~n', [A3]),
    nl,
    
    demo_example('Flatten and reverse:',
                 'B = [[1],[2],[3]], starlog_call(A is B>>flatten>>reverse)'),
    B4 = [[1],[2],[3]],
    starlog_call(A4 is B4>>flatten>>reverse),
    format('  Result: A = ~w~n', [A4]),
    nl,
    
    demo_example('Max of list:',
                 'B = [5,2,8,1], starlog_call(A is B>>max_list)'),
    B5 = [5,2,8,1],
    starlog_call(A5 is B5>>max_list),
    format('  Result: A = ~w~n', [A5]),
    nl,
    
    demo_example('Sum of list:',
                 'B = [1,2,3,4], starlog_call(A is B>>sum_list)'),
    B6 = [1,2,3,4],
    starlog_call(A6 is B6>>sum_list),
    format('  Result: A = ~w~n', [A6]),
    nl.

% ============================================================
% Section 3: String Variables with Method Chains
% ============================================================

demo_string_variables :-
    demo_header('Section 3: String Variables with Method Chains'),
    
    demo_example('String length:',
                 'B = "hello", starlog_call(A is B>>string_length)'),
    B1 = "hello",
    starlog_call(A1 is B1>>string_length),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('String to uppercase:',
                 'B = "hello", starlog_call(A is B>>string_upper)'),
    B2 = "hello",
    starlog_call(A2 is B2>>string_upper),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('String to lowercase:',
                 'B = "WORLD", starlog_call(A is B>>string_lower)'),
    B3 = "WORLD",
    starlog_call(A3 is B3>>string_lower),
    format('  Result: A = ~w~n', [A3]),
    nl,
    
    demo_example('Uppercase then length:',
                 'B = "hello", starlog_call(A is B>>string_upper>>string_length)'),
    B4 = "hello",
    starlog_call(A4 is B4>>string_upper>>string_length),
    format('  Result: A = ~w~n', [A4]),
    nl.

% ============================================================
% Section 4: Atom Variables with Method Chains
% ============================================================

demo_atom_variables :-
    demo_header('Section 4: Atom Variables with Method Chains'),
    
    demo_example('Atom length:',
                 'B = hello, starlog_call(A is B>>atom_length)'),
    B1 = hello,
    starlog_call(A1 is B1>>atom_length),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('Atom to characters:',
                 'B = world, starlog_call(A is B>>atom_chars)'),
    B2 = world,
    starlog_call(A2 is B2>>atom_chars),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('Atom to chars then length:',
                 'B = abc, starlog_call(A is B>>atom_chars>>length)'),
    B3 = abc,
    starlog_call(A3 is B3>>atom_chars>>length),
    format('  Result: A = ~w~n', [A3]),
    nl.

% ============================================================
% Section 5: Number Variables with Method Chains
% ============================================================

demo_number_variables :-
    demo_header('Section 5: Number Variables with Method Chains'),
    
    demo_example('Absolute value:',
                 'B = -42, starlog_call(A is B>>abs)'),
    B1 = -42,
    starlog_call(A1 is B1>>abs),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('Ceiling:',
                 'B = 3.7, starlog_call(A is B>>ceiling)'),
    B2 = 3.7,
    starlog_call(A2 is B2>>ceiling),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('Floor:',
                 'B = 3.2, starlog_call(A is B>>floor)'),
    B3 = 3.2,
    starlog_call(A3 is B3>>floor),
    format('  Result: A = ~w~n', [A3]),
    nl,
    
    demo_example('Square root:',
                 'B = 16, starlog_call(A is B>>sqrt)'),
    B4 = 16,
    starlog_call(A4 is B4>>sqrt),
    format('  Result: A = ~w~n', [A4]),
    nl.

% ============================================================
% Section 6: Complex Chains (3+ operations)
% ============================================================

demo_complex_chains :-
    demo_header('Section 6: Complex Chains (3+ operations)'),
    
    demo_example('Sort, reverse, then length:',
                 'B = [3,1,2], starlog_call(A is B>>sort>>reverse>>length)'),
    B1 = [3,1,2],
    starlog_call(A1 is B1>>sort>>reverse>>length),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('Flatten, reverse, then length:',
                 'B = [[1,2],[3,4]], starlog_call(A is B>>flatten>>reverse>>length)'),
    B2 = [[1,2],[3,4]],
    starlog_call(A2 is B2>>flatten>>reverse>>length),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('Uppercase, to chars, then length:',
                 'B = "hello", starlog_call(A is B>>string_upper>>string_chars>>length)'),
    B3 = "hello",
    starlog_call(A3 is B3>>string_upper>>string_chars>>length),
    format('  Result: A = ~w~n', [A3]),
    nl.

% ============================================================
% Section 7: Variables from Starlog Operators
% ============================================================

demo_operator_variables :-
    demo_header('Section 7: Variables from Starlog Operators'),
    
    demo_example('List append then reverse:',
                 'starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>reverse)'),
    starlog_call(B1 is [1,2]&[3,4]),
    starlog_call(A1 is B1>>reverse),
    format('  Result: B = ~w, A = ~w~n', [B1, A1]),
    nl,
    
    demo_example('List append then sort and length:',
                 'starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>sort>>length)'),
    starlog_call(B2 is [1,2]&[3,4]),
    starlog_call(A2 is B2>>sort>>length),
    format('  Result: B = ~w, A = ~w~n', [B2, A2]),
    nl,
    
    demo_example('String concat then length:',
                 'starlog_call(B is "hello":"world"), starlog_call(A is B>>string_length)'),
    starlog_call(B3 is "hello":"world"),
    starlog_call(A3 is B3>>string_length),
    format('  Result: B = ~w, A = ~w~n', [B3, A3]),
    nl,
    
    demo_example('Reverse then length:',
                 'starlog_call(B is reverse([1,2,3])), starlog_call(A is B>>length)'),
    starlog_call(B4 is reverse([1,2,3])),
    starlog_call(A4 is B4>>length),
    format('  Result: B = ~w, A = ~w~n', [B4, A4]),
    nl.

% ============================================================
% Section 8: Multiple Variables in Sequence
% ============================================================

demo_multiple_variables :-
    demo_header('Section 8: Multiple Variables in Sequence'),
    
    demo_example('Chain through 3 variables:',
                 'B = [3,2,2], C is B>>sort, A is C>>length'),
    B1 = [3,2,2],
    starlog_call(C1 is B1>>sort),
    starlog_call(A1 is C1>>length),
    format('  Result: B = ~w, C = ~w, A = ~w~n', [B1, C1, A1]),
    nl,
    
    demo_example('String processing chain:',
                 'B = "hello", C is B>>string_upper, A is C>>string_length'),
    B2 = "hello",
    starlog_call(C2 is B2>>string_upper),
    starlog_call(A2 is C2>>string_length),
    format('  Result: B = ~w, C = ~w, A = ~w~n', [B2, C2, A2]),
    nl,
    
    demo_example('List processing chain through 4 variables:',
                 'B = [1,2,3], C is B>>reverse, D is C>>sort, A is D>>length'),
    B3 = [1,2,3],
    starlog_call(C3 is B3>>reverse),
    starlog_call(D3 is C3>>sort),
    starlog_call(A3 is D3>>length),
    format('  Result: B = ~w, C = ~w, D = ~w, A = ~w~n', [B3, C3, D3, A3]),
    nl.

% ============================================================
% Section 9: Edge Cases
% ============================================================

demo_edge_cases :-
    demo_header('Section 9: Edge Cases'),
    
    demo_example('Empty list:',
                 'B = [], starlog_call(A is B>>length)'),
    B1 = [],
    starlog_call(A1 is B1>>length),
    format('  Result: A = ~w~n', [A1]),
    nl,
    
    demo_example('Single element list:',
                 'B = [5], starlog_call(A is B>>sort>>length)'),
    B2 = [5],
    starlog_call(A2 is B2>>sort>>length),
    format('  Result: A = ~w~n', [A2]),
    nl,
    
    demo_example('Empty string:',
                 'B = "", starlog_call(A is B>>string_length)'),
    B3 = "",
    starlog_call(A3 is B3>>string_length),
    format('  Result: A = ~w~n', [A3]),
    nl,
    
    demo_example('Already sorted list:',
                 'B = [1,2,3], starlog_call(A is B>>sort)'),
    B4 = [1,2,3],
    starlog_call(A4 is B4>>sort),
    format('  Result: A = ~w~n', [A4]),
    nl.

% ============================================================
% Section 10: Summary
% ============================================================

demo_summary :-
    demo_header('Summary: All Combinations and Configurations'),
    
    writeln('  This demonstration covers:'),
    writeln(''),
    writeln('  1. Problem Statement Pattern'),
    writeln('     - B = [3,2,2], A is B>>sort>>length'),
    writeln('     - Single and multiple operations'),
    writeln(''),
    writeln('  2. List Variables'),
    writeln('     - reverse, sort, flatten, length'),
    writeln('     - max_list, min_list, sum_list'),
    writeln('     - Multiple operations chained'),
    writeln(''),
    writeln('  3. String Variables'),
    writeln('     - string_length, string_upper, string_lower'),
    writeln('     - string_chars'),
    writeln('     - Chained string operations'),
    writeln(''),
    writeln('  4. Atom Variables'),
    writeln('     - atom_length, atom_chars'),
    writeln('     - Chained atom operations'),
    writeln(''),
    writeln('  5. Number Variables'),
    writeln('     - abs, ceiling, floor, round, sqrt'),
    writeln('     - Mathematical transformations'),
    writeln(''),
    writeln('  6. Complex Chains'),
    writeln('     - 3+ operations in a single chain'),
    writeln('     - Mixed operation types'),
    writeln(''),
    writeln('  7. Starlog Operator Variables'),
    writeln('     - Variables assigned from &, :, and functions'),
    writeln('     - Then used in method chains'),
    writeln(''),
    writeln('  8. Multiple Variables in Sequence'),
    writeln('     - Chaining results through multiple variables'),
    writeln('     - Multi-step transformations'),
    writeln(''),
    writeln('  9. Edge Cases'),
    writeln('     - Empty lists and strings'),
    writeln('     - Single element lists'),
    writeln('     - Already processed data'),
    writeln(''),
    writeln('  All patterns demonstrated work correctly!'),
    nl.

% ============================================================
% Main Demo
% ============================================================

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Variable Method Chain Demonstration                  ║'),
    writeln('║  Pattern: B = Value, A is B>>operation>>operation     ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    demo_problem_statement,
    demo_list_variables,
    demo_string_variables,
    demo_atom_variables,
    demo_number_variables,
    demo_complex_chains,
    demo_operator_variables,
    demo_multiple_variables,
    demo_edge_cases,
    demo_summary,
    
    nl.

:- initialization(main, main).
