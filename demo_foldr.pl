% demo_foldr.pl
% Demonstration of foldr implementation with various configurations

:- use_module(starlog).

demo_foldr :-
    write('═══════════════════════════════════════════════════════'), nl,
    write('  FOLDR IMPLEMENTATION DEMO'), nl,
    write('═══════════════════════════════════════════════════════'), nl,
    nl,
    
    % Configuration 1: Pure Prolog syntax
    write('Configuration 1: Pure Prolog'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    string_chars("abc", Chars1),
    reverse(Chars1, RevChars1),
    foldr(string_concat, RevChars1, "", Result1),
    format('  foldr(string_concat, reverse(string_chars("abc")), "", B)~n'),
    format('  B = "~w"~n', [Result1]),
    (Result1 = "cba" -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    % Configuration 2: With Starlog syntax using starlog_call
    write('Configuration 2: Starlog syntax with starlog_call'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    starlog_call(Result2 is foldr(string_concat, reverse(string_chars("abc")), "")),
    format('  starlog_call(B is foldr(string_concat, reverse(string_chars("abc")), ""))~n'),
    format('  B = "~w"~n', [Result2]),
    (Result2 = "cba" -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    % Configuration 3: Step-by-step evaluation
    write('Configuration 3: Step-by-step evaluation'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    format('  Step 1: string_chars("abc", Chars)~n'),
    string_chars("abc", Chars3),
    format('          Chars = ~w~n', [Chars3]),
    format('  Step 2: reverse(Chars, RevChars)~n'),
    reverse(Chars3, RevChars3),
    format('          RevChars = ~w~n', [RevChars3]),
    format('  Step 3: foldr(string_concat, RevChars, "", B)~n'),
    foldr(string_concat, RevChars3, "", Result3),
    format('          B = "~w"~n', [Result3]),
    (Result3 = "cba" -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    % Configuration 4: Using nested Starlog expressions
    write('Configuration 4: Nested Starlog expressions'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    starlog_call(Chars4 is string_chars("abc")),
    starlog_call(RevChars4 is reverse(Chars4)),
    starlog_call(Result4 is foldr(string_concat, RevChars4, "")),
    format('  Chars4 is string_chars("abc") -> ~w~n', [Chars4]),
    format('  RevChars4 is reverse(Chars4) -> ~w~n', [RevChars4]),
    format('  Result4 is foldr(string_concat, RevChars4, "") -> "~w"~n', [Result4]),
    (Result4 = "cba" -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    % Configuration 5: Different strings to show generality
    write('Configuration 5: Different input strings'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    test_string("hello", "olleh"),
    test_string("world", "dlrow"),
    test_string("x", "x"),
    test_string("", ""),
    nl,
    
    % Configuration 6: Using foldr with other operations
    write('Configuration 6: foldr with list append'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    foldr(append, [[1], [2], [3]], [], Result6),
    format('  foldr(append, [[1], [2], [3]], [], Result)~n'),
    format('  Result = ~w~n', [Result6]),
    (Result6 = [1,2,3] -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    % Configuration 7: Using Starlog with append
    write('Configuration 7: Starlog with foldr and append'), nl,
    write('───────────────────────────────────────────────────────'), nl,
    starlog_call(Result7 is foldr(append, reverse([[1,2], [3,4], [5]]), [])),
    format('  Result is foldr(append, reverse([[1,2], [3,4], [5]]), [])~n'),
    format('  Result = ~w~n', [Result7]),
    (Result7 = [5,3,4,1,2] -> write('  ✓ CORRECT'), nl, nl ; write('  ✗ FAILED'), nl, nl),
    
    write('═══════════════════════════════════════════════════════'), nl,
    write('  ALL CONFIGURATIONS DEMONSTRATED'), nl,
    write('═══════════════════════════════════════════════════════'), nl.

% Helper to test different strings
test_string(Input, Expected) :-
    starlog_call(Result is foldr(string_concat, reverse(string_chars(Input)), "")),
    format('  foldr(string_concat, reverse(string_chars("~w")), "") = "~w"', [Input, Result]),
    (Result = Expected -> 
        write(' ✓'), nl
    ; 
        format(' ✗ (expected "~w")~n', [Expected])
    ).

:- initialization(demo_foldr, main).
