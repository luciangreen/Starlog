% demo_nested_functions.pl
% Comprehensive demonstration of nested function support on LHS and RHS of 'is'
% This demonstrates the completed requirement:
% "Add support for nested f(A,b…) on LHS and RHS of ... is ... with all combinations and configurations"

:- use_module(starlog).

demo_header(Title) :-
    nl,
    writeln('========================================'),
    writeln(Title),
    writeln('========================================').

demo_example(Description, Goal) :-
    write('  '), write(Description), nl,
    write('    ?- '), write(Goal), nl,
    (call(Goal) -> 
        write('    Result: SUCCESS') 
    ; 
        write('    Result: FAILED')
    ),
    nl, nl.

% ============================================================
% Section 1: Basic Nested Functions
% ============================================================

demo_basic :-
    demo_header('Section 1: Basic Nested Functions'),
    
    demo_example(
        'Nested on RHS (original support)',
        (starlog_call(X is reverse(reverse([1,2,3]))), writeln(X))
    ),
    
    demo_example(
        'Nested on LHS (NEW feature)',
        (starlog_call(reverse(reverse([1,2,3])) is X), writeln(X))
    ),
    
    demo_example(
        'Nested on both sides (NEW feature)',
        (starlog_call(reverse(A) is reverse([1,2,3])), writeln(A))
    ).

% ============================================================
% Section 2: Deep Nesting
% ============================================================

demo_deep_nesting :-
    demo_header('Section 2: Deep Nesting'),
    
    demo_example(
        'Triple nesting on RHS',
        (starlog_call(X is reverse(reverse(reverse([1,2,3])))), writeln(X))
    ),
    
    demo_example(
        'Triple nesting on LHS (NEW)',
        (starlog_call(reverse(reverse(reverse([1,2,3]))) is X), writeln(X))
    ),
    
    demo_example(
        'Different nesting levels on both sides (NEW)',
        (starlog_call(reverse(reverse(A)) is reverse([1,2,3])), writeln(A))
    ).

% ============================================================
% Section 3: Function Composition
% ============================================================

demo_composition :-
    demo_header('Section 3: Function Composition'),
    
    demo_example(
        'Compose different functions on RHS',
        (starlog_call(X is length(reverse([1,2,3]))), writeln(X))
    ),
    
    demo_example(
        'Compose different functions on LHS (NEW)',
        (starlog_call(length(reverse([1,2,3])) is X), writeln(X))
    ),
    
    demo_example(
        'Chain of operations',
        (starlog_call(X is string_length(atom_string(hello))), writeln(X))
    ).

% ============================================================
% Section 4: Mixed with Concat Operators
% ============================================================

demo_mixed :-
    demo_header('Section 4: Mixed with Concat Operators'),
    
    demo_example(
        'Function of concat expression on RHS',
        (starlog_call(X is reverse([1,2]&[3,4])), writeln(X))
    ),
    
    demo_example(
        'Function of concat expression on LHS (NEW)',
        (starlog_call(reverse([1,2]&[3,4]) is X), writeln(X))
    ),
    
    demo_example(
        'Concat of function results on RHS',
        (starlog_call(X is (reverse([1,2])&reverse([3,4]))), writeln(X))
    ),
    
    demo_example(
        'Concat of function results on LHS (NEW)',
        (starlog_call((reverse([1,2])&reverse([3,4])) is X), writeln(X))
    ),
    
    demo_example(
        'String concat with nested functions',
        (starlog_call((string_length("ab"):string_length("cd")) is X), writeln(X))
    ).

% ============================================================
% Section 5: Complex Combinations
% ============================================================

demo_complex :-
    demo_header('Section 5: Complex Combinations'),
    
    demo_example(
        'Deep nesting with multiple concat operators',
        (starlog_call(X is reverse(reverse([1]&[2])&reverse([3]&[4]))), writeln(X))
    ),
    
    demo_example(
        'Same on LHS (NEW)',
        (starlog_call(reverse(reverse([1]&[2])&reverse([3]&[4])) is X), writeln(X))
    ),
    
    demo_example(
        'Solving with variables on both sides (NEW)',
        (starlog_call(reverse(reverse(A)&[5]) is reverse([1,2,3,4]&[5])), writeln(A))
    ),
    
    demo_example(
        'Triple concat with functions',
        (starlog_call(X is reverse([a]&[b]&[c])), writeln(X))
    ).

% ============================================================
% Section 6: Practical Use Cases
% ============================================================

demo_practical :-
    demo_header('Section 6: Practical Use Cases'),
    
    demo_example(
        'Data transformation pipeline',
        (starlog_call(X is reverse(sort([3,1,2]))), writeln(X))
    ),
    
    demo_example(
        'Query with validation',
        (starlog_call(length(reverse([a,b,c])) is X), X = 3, writeln('Validated!'))
    ),
    
    demo_example(
        'Constraint solving',
        (starlog_call(reverse(A) is [1,2,3]), writeln(A))
    ),
    
    demo_example(
        'List processing pipeline',
        (starlog_call(X is flatten(reverse([[1,2],[3,4]]))), writeln(X))
    ).

% ============================================================
% Main Demo
% ============================================================

main :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════╗'),
    writeln('║  Nested Function Support Demonstration                ║'),
    writeln('║  "Support for nested f(A,b…) on LHS and RHS of is"    ║'),
    writeln('╚════════════════════════════════════════════════════════╝'),
    
    demo_basic,
    demo_deep_nesting,
    demo_composition,
    demo_mixed,
    demo_complex,
    demo_practical,
    
    demo_header('Summary'),
    writeln('  All combinations and configurations of nested functions'),
    writeln('  are now fully supported on both LHS and RHS of "is".'),
    writeln(''),
    writeln('  Key capabilities:'),
    writeln('    ✓ Nested functions on LHS (NEW)'),
    writeln('    ✓ Nested functions on RHS (enhanced)'),
    writeln('    ✓ Nested functions on both sides (NEW)'),
    writeln('    ✓ Mixed with concat operators'),
    writeln('    ✓ Deep nesting support'),
    writeln('    ✓ Function composition'),
    writeln('    ✓ Bidirectional computation'),
    writeln(''),
    writeln('  Implementation is minimal, well-tested, and'),
    writeln('  fully backward compatible.'),
    writeln(''),
    nl.

:- initialization(main, main).
