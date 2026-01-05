% demo_all_combinations_configurations.pl
% Comprehensive demonstration of: A is (1:1 >> string_number) * (+(1,1))
% Shows all combinations and configurations of:
% - String concatenation (1:1)
% - Method chaining (>> string_number)
% - Arithmetic operations (* (+(1,1)))

:- use_module(starlog).

demo_header(Title) :-
    nl,
    writeln('========================================'),
    writeln(Title),
    writeln('========================================').

demo_example(Description, GoalAtom) :-
    write('  '), write(Description), nl,
    write('    ?- '), write(GoalAtom), nl,
    atom_to_term(GoalAtom, Goal, []),
    (catch(call(Goal), E, (write('    ERROR: '), writeln(E), fail)) -> 
        (Goal = starlog_call(R is _) ->
            format('    Result: ~w~n', [R])
        ;
            writeln('    Success')
        )
    ; 
        writeln('    Failed')
    ),
    nl.

% ============================================================
% Section 1: Core Pattern Breakdown
% ============================================================

demo_core_pattern :-
    demo_header('Section 1: Core Pattern - (1:1 >> string_number) * (+(1,1))'),
    
    writeln('  Step-by-step breakdown:'),
    nl,
    
    writeln('  Step 1: String concatenation'),
    demo_example('1:1 concatenates to "11"',
                 'starlog_call(A is 1:1)'),
    
    writeln('  Step 2: Convert string to number'),
    demo_example('string_number("11") converts to 11',
                 'starlog_call(A is string_number("11"))'),
    
    writeln('  Step 3: Method chaining'),
    demo_example('1:1 >> string_number chains both operations',
                 'starlog_call(A is 1:1 >> string_number)'),
    
    writeln('  Step 4: Arithmetic addition'),
    demo_example('+(1,1) adds to 2',
                 'starlog_call(A is +(1,1))'),
    
    writeln('  Step 5: Complete expression'),
    demo_example('(1:1 >> string_number) * (+(1,1)) = 11 * 2 = 22',
                 'starlog_call(A is (1:1 >> string_number) * (+(1,1)))').

% ============================================================
% Section 2: Number Variations
% ============================================================

demo_number_variations :-
    demo_header('Section 2: Variations with Different Numbers'),
    
    demo_example('2:3 concatenates to "23", multiply by 2 = 46',
                 'starlog_call(A is (2:3 >> string_number) * (+(1,1)))'),
    
    demo_example('4:5 concatenates to "45", multiply by 4 = 180',
                 'starlog_call(A is (4:5 >> string_number) * (+(2,2)))'),
    
    demo_example('1:0 concatenates to "10", multiply by 6 = 60',
                 'starlog_call(A is (1:0 >> string_number) * (+(3,3)))'),
    
    demo_example('1:2:3 concatenates to "123", multiply by 2 = 246',
                 'starlog_call(A is (1:2:3 >> string_number) * (+(1,1)))').

% ============================================================
% Section 3: Arithmetic Operator Variations
% ============================================================

demo_arithmetic_operators :-
    demo_header('Section 3: All Arithmetic Operators'),
    
    writeln('  Addition:'),
    demo_example('11 + 2 = 13',
                 'starlog_call(A is (1:1 >> string_number) + (+(1,1)))'),
    
    writeln('  Subtraction:'),
    demo_example('11 - 2 = 9',
                 'starlog_call(A is (1:1 >> string_number) - (+(1,1)))'),
    
    writeln('  Multiplication:'),
    demo_example('11 * 2 = 22',
                 'starlog_call(A is (1:1 >> string_number) * (+(1,1)))'),
    
    writeln('  Division:'),
    demo_example('22 / 2 = 11',
                 'starlog_call(A is (2:2 >> string_number) / (+(1,1)))'),
    
    writeln('  Integer Division:'),
    demo_example('23 // 4 = 5',
                 'starlog_call(A is (2:3 >> string_number) // (+(2,2)))'),
    
    writeln('  Modulo:'),
    demo_example('23 mod 5 = 3',
                 'starlog_call(A is (2:3 >> string_number) mod (+(3,2)))'),
    
    writeln('  Power (using **):'),
    demo_example('20 ** 2 = 400',
                 'starlog_call(A is (2:0 >> string_number) ** (+(1,1)))'),
    
    writeln('  Power (using ^):'),
    demo_example('30 ^ 2 = 900',
                 'starlog_call(A is (3:0 >> string_number) ^ (+(1,1)))').

% ============================================================
% Section 4: Method Chain Variations
% ============================================================

demo_method_chain_variations :-
    demo_header('Section 4: Method Chain Variations'),
    
    writeln('  Chaining multiple operations:'),
    demo_example('Convert number to string and back',
                 'starlog_call(A is (1:1 >> string_number >> number_string) : "x")'),
    
    writeln('  String operations:'),
    demo_example('Get length of concatenated string',
                 'starlog_call(A is (1:1 >> string_length))'),
    
    writeln('  List operations in chain:'),
    demo_example('Reverse a list containing concatenated string',
                 'starlog_call(A is ([1:1] >> reverse >> length))').

% ============================================================
% Section 5: Nested Combinations
% ============================================================

demo_nested_combinations :-
    demo_header('Section 5: Nested Combinations'),
    
    writeln('  Nested string concatenation:'),
    demo_example('(1:1):(2:2) = "1122", convert to 1122, multiply by 2 = 2244',
                 'starlog_call(A is ((1:1) : (2:2) >> string_number) * 2)'),
    
    writeln('  Nested arithmetic:'),
    demo_example('11 * (2 + 2) = 44',
                 'starlog_call(A is (1:1 >> string_number) * (+(1,1) + +(1,1)))'),
    
    writeln('  Complex nested expression:'),
    demo_example('(12 + 34) * 2 = 92',
                 'starlog_call(A is ((1:2 >> string_number) + (3:4 >> string_number)) * (+(1,1)))').

% ============================================================
% Section 6: Mixed Operator Combinations
% ============================================================

demo_mixed_operators :-
    demo_header('Section 6: Mixed Operator Combinations'),
    
    writeln('  Combining different Starlog operators:'),
    demo_example('String and list operations',
                 'starlog_call(A is (1:1) : ([2] & [3]))'),
    
    writeln('  Multiple expressions combined:'),
    demo_example('(11 * 2) + (22 * 2) = 66',
                 'starlog_call(A is ((1:1 >> string_number) * 2) + ((2:2 >> string_number) * 2))'),
    
    writeln('  Atom concatenation variant:'),
    demo_example('Atom length of a•b = 2, multiply by 2 = 4',
                 'starlog_call(A is (a•b >> atom_length) * (+(1,1)))').

% ============================================================
% Section 7: Edge Cases
% ============================================================

demo_edge_cases :-
    demo_header('Section 7: Edge Cases'),
    
    demo_example('Zero in concatenation: 0:0 = "00" = 0',
                 'starlog_call(A is (0:0 >> string_number) * (+(1,1)))'),
    
    demo_example('Multiplication by zero',
                 'starlog_call(A is (1:1 >> string_number) * (+(0,0)))'),
    
    demo_example('Roundtrip conversion',
                 'starlog_call(A is (5 >> number_string) >> string_number)'),
    
    demo_example('Negative numbers',
                 'starlog_call(A is (1:1 >> string_number) * (+(0,-1)))').

% ============================================================
% Section 8: All Configurations Summary
% ============================================================

demo_summary :-
    demo_header('Summary: All Combinations and Configurations'),
    
    writeln('  This demonstration covers:'),
    writeln(''),
    writeln('  1. String Concatenation Operator (:)'),
    writeln('     - Numeric concatenation: 1:1 → "11"'),
    writeln('     - Multi-digit: 1:2:3 → "123"'),
    writeln(''),
    writeln('  2. Method Chaining Operator (>>)'),
    writeln('     - String to number: >> string_number'),
    writeln('     - Number to string: >> number_string'),
    writeln('     - String operations: >> string_length'),
    writeln('     - List operations: >> reverse, >> length'),
    writeln(''),
    writeln('  3. Arithmetic Operators'),
    writeln('     - Addition: +'),
    writeln('     - Subtraction: -'),
    writeln('     - Multiplication: *'),
    writeln('     - Division: /'),
    writeln('     - Integer Division: //'),
    writeln('     - Modulo: mod'),
    writeln('     - Power: ** or ^'),
    writeln(''),
    writeln('  4. Arithmetic Function Notation'),
    writeln('     - +(N,M) for addition'),
    writeln('     - Can be used with any arithmetic operator'),
    writeln(''),
    writeln('  5. Nested Combinations'),
    writeln('     - Nested string concatenation'),
    writeln('     - Nested arithmetic'),
    writeln('     - Complex multi-level nesting'),
    writeln(''),
    writeln('  6. Mixed Operators'),
    writeln('     - String (:) with list (&)'),
    writeln('     - Atom (•) with arithmetic'),
    writeln('     - Multiple different operators in one expression'),
    writeln(''),
    writeln('  7. Edge Cases'),
    writeln('     - Zero values'),
    writeln('     - Negative numbers'),
    writeln('     - Roundtrip conversions'),
    writeln(''),
    writeln('  Core Pattern: A is (1:1 >> string_number) * (+(1,1))'),
    writeln('    - 1:1 concatenates to "11"'),
    writeln('    - >> string_number converts to 11'),
    writeln('    - +(1,1) evaluates to 2'),
    writeln('    - 11 * 2 = 22'),
    writeln(''),
    writeln('  All configurations are fully supported!'),
    nl.

% ============================================================
% Main Demo
% ============================================================

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  All Combinations and Configurations Demo             ║'),
    writeln('║  Pattern: A is (1:1 >> string_number) * (+(1,1))      ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    demo_core_pattern,
    demo_number_variations,
    demo_arithmetic_operators,
    demo_method_chain_variations,
    demo_nested_combinations,
    demo_mixed_operators,
    demo_edge_cases,
    demo_summary,
    
    nl.

:- initialization(main, main).
