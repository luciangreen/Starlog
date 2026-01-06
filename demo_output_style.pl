% demo_output_style.pl
% Demonstration of the output_style option for starlog_output_code
% This shows how to convert between nested calls and method chaining

:- use_module(starlog).

demo_header(Title) :-
    nl,
    writeln('========================================'),
    writeln(Title),
    writeln('========================================'),
    nl.

% ============================================================
% Section 1: Basic Transformations
% ============================================================

demo_basic :-
    demo_header('Section 1: Basic Transformations'),
    
    writeln('Converting method chains to nested calls:'),
    writeln('  Input:  X is reverse([1,2,3]) >> length'),
    starlog_output_code((X is reverse([1,2,3]) >> length), _, 
        [output_style(nested_calls), print(true)]),
    nl,
    
    writeln('Converting nested calls to method chains:'),
    writeln('  Input:  X is length(reverse([1,2,3]))'),
    starlog_output_code((X is length(reverse([1,2,3]))), _, 
        [output_style(method_chaining), print(true)]),
    nl.

% ============================================================
% Section 2: Multi-Level Transformations
% ============================================================

demo_multi_level :-
    demo_header('Section 2: Multi-Level Transformations'),
    
    writeln('Deep chain to nested:'),
    writeln('  Input:  X is sort([3,1,2]) >> reverse >> length'),
    starlog_output_code((X is sort([3,1,2]) >> reverse >> length), _, 
        [output_style(nested_calls), print(true)]),
    nl,
    
    writeln('Deep nested to chain:'),
    writeln('  Input:  X is length(reverse(sort([3,1,2])))'),
    starlog_output_code((X is length(reverse(sort([3,1,2])))), _, 
        [output_style(method_chaining), print(true)]),
    nl.

% ============================================================
% Section 3: With Starlog Operators
% ============================================================

demo_with_operators :-
    demo_header('Section 3: With Starlog Operators'),
    
    writeln('Chain with list append:'),
    writeln('  Input:  X is ([1,2]&[3,4]) >> reverse >> length'),
    starlog_output_code((X is ([1,2]&[3,4]) >> reverse >> length), _, 
        [output_style(nested_calls), print(true)]),
    nl,
    
    writeln('Nested with list append:'),
    writeln('  Input:  X is length(reverse([1,2]&[3,4]))'),
    starlog_output_code((X is length(reverse([1,2]&[3,4]))), _, 
        [output_style(method_chaining), print(true)]),
    nl,
    
    writeln('Chain with string concat:'),
    writeln('  Input:  X is ("hello":"world") >> string_length'),
    starlog_output_code((X is ("hello":"world") >> string_length), _, 
        [output_style(nested_calls), print(true)]),
    nl.

% ============================================================
% Section 4: Roundtrip Conversion
% ============================================================

demo_roundtrip :-
    demo_header('Section 4: Roundtrip Conversion'),
    
    writeln('Demonstrating that conversions are reversible:'),
    nl,
    
    % Original chain
    writeln('Original: X is [1,2,3] >> reverse >> sort >> length'),
    
    % Chain to nested
    starlog_output_code((X is [1,2,3] >> reverse >> sort >> length), 
        Code1, [output_style(nested_calls)]),
    write('Nested:   '), write(Code1), nl,
    
    % Nested back to chain
    starlog_output_code(Code1, Code2, [output_style(method_chaining)]),
    write('Chain:    '), write(Code2), nl,
    nl,
    
    writeln('✓ Roundtrip preserves the structure!'),
    nl.

% ============================================================
% Section 5: Combining with Other Options
% ============================================================

demo_combinations :-
    demo_header('Section 5: Combining with Other Options'),
    
    writeln('Using output_style with compression:'),
    writeln('  Input (Prolog): string_concat("a", "b", T1), string_concat(T1, "c", T2)'),
    starlog_output_code(
        (string_concat("a", "b", T1), string_concat(T1, "c", T2)), _, 
        [compress(true), output_style(method_chaining), print(true)]),
    nl,
    
    writeln('Using output_style with rename:'),
    writeln('  Input:  X is reverse([1,2,3]) >> length'),
    starlog_output_code((X is reverse([1,2,3]) >> length), _, 
        [output_style(nested_calls), rename(true), print(true)]),
    nl.

% ============================================================
% Section 6: Using with Files
% ============================================================

demo_files :-
    demo_header('Section 6: Using with Files'),
    
    % Create a sample file
    writeln('Creating sample Starlog file...'),
    open('/tmp/sample.pl', write, Stream),
    write(Stream, '% Sample Starlog code\n'),
    write(Stream, 'process(List, Result) :- Result is List >> reverse >> sort.\n'),
    write(Stream, 'count(List, Len) :- Len is List >> length.\n'),
    close(Stream),
    writeln('  Written to /tmp/sample.pl'),
    nl,
    
    writeln('Reading file with output_style(nested_calls):'),
    starlog_output_file('/tmp/sample.pl', user_output, [output_style(nested_calls)]),
    nl,
    
    writeln('Reading file with output_style(method_chaining):'),
    starlog_output_file('/tmp/sample.pl', user_output, [output_style(method_chaining)]),
    nl.

% ============================================================
% Main Demo
% ============================================================

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Output Style Option Demonstration                    ║'),
    writeln('║  Convert between nested calls and method chaining     ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    demo_basic,
    demo_multi_level,
    demo_with_operators,
    demo_roundtrip,
    demo_combinations,
    demo_files,
    
    demo_header('Summary'),
    writeln('The output_style option provides two transformation modes:'),
    writeln('  • output_style(nested_calls) - Convert chains to nested'),
    writeln('  • output_style(method_chaining) - Convert nested to chains'),
    nl,
    writeln('Key features:'),
    writeln('  ✓ Works with multi-level nesting/chaining'),
    writeln('  ✓ Handles Starlog operators (:, &, •)'),
    writeln('  ✓ Reversible transformations'),
    writeln('  ✓ Combines with other options (compress, rename)'),
    writeln('  ✓ Works with both code and file operations'),
    nl,
    halt.

:- initialization(main, main).
