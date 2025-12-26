# Starlog-in-Prolog Documentation

**Date**: 2025-12-24  
**Author**: luciangreenPlease

## Overview

Starlog-in-Prolog is a library that allows developers to write Starlog syntax directly inside normal `.pl` files and run it immediately, without converting files. Starlog is a variant of Prolog that uses a more functional notation, particularly for built-in predicates where the output parameter is represented using an "is" operator rather than as the last argument of a predicate.

## What is Starlog?

Starlog is a Prolog variant that uses the notation `Result is function(Args)` instead of Prolog's `function(Args, Result)`. This makes code more readable when working with transformations and operations that produce a result, as the output variable appears first in the expression rather than at the end of a parameter list.

With Starlog-in-Prolog, you can write Starlog syntax directly in your Prolog source files, and the library will automatically expand it into equivalent Prolog goals at load-time using goal_expansion and term_expansion hooks.

## Quick Start

### Basic Usage in a File

```prolog
:- use_module(starlog_in_prolog).

% Write Starlog syntax directly in your code
test(Result) :- 
    Result is "Hello" : " " : "World".

% It automatically expands to:
% test(Result) :- 
%     string_concat("Hello", " ", _G1),
%     string_concat(_G1, "World", Result).
```

### Interactive Use in REPL

For direct typing in the REPL, use `starlog_call/1`:

```prolog
?- use_module(starlog_in_prolog).
?- starlog_call(A is "x":"y").
A = "xy".

?- starlog_call(L is [1] & [2]).
L = [1, 2].
```

### Saving Results to Variables

The library provides predicates to explicitly save call results to variables:

#### starlog_call/2 - Execute and Save Result

Execute a Starlog goal and explicitly return the result in a variable:

```prolog
?- starlog_call(X is "hello":"world", Result).
Result = "helloworld".

?- starlog_call(Y is [1,2] & [3,4], Result).
Result = [1, 2, 3, 4].

?- starlog_call(Z is reverse([1,2,3]), Result).
Result = [3, 2, 1].
```

#### starlog_eval/2 - Evaluate Expression

Evaluate a Starlog expression and return the result:

```prolog
?- starlog_eval("x":"y", Result).
Result = "xy".

?- starlog_eval(1+1, Result).
Result = 2.

?- starlog_eval([a] & [b,c], Result).
Result = [a, b, c].
```

#### starlog_no_eval/2 - Preserve Expression

Preserve a Starlog expression without evaluation:

```prolog
?- starlog_no_eval(1+1, Result).
Result = 1+1.

?- starlog_no_eval("hello":"world", Result).
Result = "hello":"world".

?- starlog_no_eval([a] & [b], Result).
Result = [a] & [b].
```

These predicates are useful for:
- Explicitly capturing results for later use
- Building symbolic expressions as data structures
- Template systems and meta-programming
- Functional composition patterns

## Starlog Syntax

**Important**: By default, Starlog evaluates all expressions. The `eval()` function is only needed when you want to force evaluation inside `no_eval()` contexts.

The library supports the following Starlog patterns:

### Special Operators

Special operators are used for common operations:
- **String concatenation**: `C is (A : B)` - expands to `string_concat(A, B, C)`
- **List append**: `C is (A & B)` - expands to `append(A, B, C)`
- **Atom concatenation**: `C is (A • B)` - expands to `atom_concat(A, B, C)`
- **List to term**: `T is ..=([f,0,1])` - expands to `T =.. [f,0,1]` (creates term `f(0,1)`)
- **Term to list**: `L is =..(f(0,1))` - expands to `f(0,1) =.. L` (creates list `[f,0,1]`)

### Expression Evaluation and Preservation

**By default, Starlog evaluates all expressions.** This is the standard behavior:

```prolog
A is 1+1              % A = 2 (arithmetic evaluated)
B is "x":"y"          % B = "xy" (concatenation evaluated)
C is [1] & [2]        % C = [1,2] (append evaluated)
```

#### Explicit Evaluation with eval

The `eval/1` function explicitly marks an expression for evaluation. While this is the default behavior, it becomes useful when you need to force evaluation inside `no_eval` contexts:

```prolog
% Normal evaluation (eval is implicit/default)
A is 1+1              % A = 2

% Explicit eval (same result)
B is eval(1+1)        % B = 2

% Force evaluation inside no_eval
C is no_eval(eval(1+1))         % C = 2 (inner eval forces evaluation)
D is no_eval("x" : eval("y":"z"))  % D = "x":"yz" (nested eval)
```

#### Expression Preservation with no_eval

The `no_eval/1` function prevents evaluation of expressions, preserving them as data:

```prolog
% Preserve arithmetic expressions
A is no_eval(1+1)          % A = 1+1 (not 2)

% Preserve Starlog operators
B is no_eval("x":"y")      % B = "x":"y" (not "xy")

% Preserve complex expressions
C is no_eval((1+2)*(3+4))  % C = (1+2)*(3+4) (not 21)

% Use eval to selectively evaluate parts
D is no_eval(eval(1+1) + 3)  % D = 2 + 3 (only 1+1 is evaluated)
```

This is useful for:
- Storing formulas as data structures
- Manipulating expressions symbolically
- Lazy evaluation patterns
- Template systems
- Selectively evaluating parts of expressions with `eval`

### Value-Returning Builtins

Many Prolog predicates can be written in Starlog syntax:
- `Length is string_length("hello")` → `string_length("hello", Length)`
- `Rev is reverse([1,2,3])` → `reverse([1,2,3], Rev)`
- `Upper is string_upper("hello")` → `string_upper("hello", Upper)`

### Nested Expressions

Nested expressions are automatically decomposed into sequential goals:

```prolog
% Starlog:
E is (A:(B:(D • F))) • C

% Expands to:
atom_concat(D, F, _G1),
string_concat(B, _G1, _G2),
string_concat(A, _G2, _G3),
atom_concat(_G3, C, E).
```

### Arithmetic is Preserved

Standard arithmetic expressions are left unchanged:

```prolog
X is 1+2      % Treated as arithmetic, not Starlog
X is Y * 5    % Also arithmetic
```

## When is/2 is Treated as Starlog vs Arithmetic

The library distinguishes between Starlog and arithmetic based on the right-hand side:

- **Starlog**: `Out is (A : B)`, `Out is func(Args)`, `Out is (A & B)`, `Out is (A • B)`, `Out is no_eval(Expr)`, `Out is eval(Expr)`
- **Arithmetic**: `Out is 1+2`, `Out is X*Y`, `Out is sqrt(N)` (when sqrt/1 is arithmetic)

**Note**: `eval` is the default behavior for Starlog expressions. You only need to use `eval()` explicitly when forcing evaluation inside `no_eval()` contexts.

## Supported Built-in Predicates

The library supports automatic expansion for many built-in predicates. Here are the main categories:

### String and Atom Operations
- `string_length/1`, `atom_length/1`
- `string_chars/1`, `atom_chars/1`
- `string_upper/1`, `string_lower/1`
- `atom_codes/1`, `string_codes/1`
- `term_string/1`, `term_to_atom/1`
- `sub_string/4`, `sub_atom/4`
- And more...

### List Operations
- `reverse/1`, `sort/1`, `flatten/1`
- `length/1`, `member/1`
- `intersection/2`, `union/2`, `subtract/2`
- `nth0/2`, `nth1/2`, `last/1`
- `min_list/1`, `max_list/1`, `sum_list/1`
- And more...

### Math Operations  
- `ceiling/1`, `floor/1`, `round/1`, `truncate/1`
- `abs/1`, `sign/1`, `sqrt/1`
- `sin/1`, `cos/1`, `tan/1`
- `log/1`, `exp/1`
- And more...

### Other Operations
- `findall/2`
- `term_variables/1`
- `split_string/3`
- `date/0`, `get_time/0` (nullary operations)
- And more...

For a complete list, see `starlog_registry.pl`.

## Extending Starlog

You can register your own value-returning builtins:

```prolog
% Register foo/2 as a value builtin
% Now: Out is foo(A,B) expands to foo(A,B,Out)
?- starlog_register_value_builtin(foo, 2, foo).

% Register with a different Prolog predicate name
?- starlog_register_value_builtin(bar, 1, my_bar).
% Now: Out is bar(X) expands to my_bar(X,Out)
```

## Outputting Starlog Code

The library provides features to convert Prolog code back to Starlog notation with human-friendly variable names (A, B, C, A1, B1, etc.) and **pretty printing with proper indentation** for nested calls and logical control structures.

### Pretty Printed Output

The output automatically formats code with indentation for:
- Nested calls (findall, and, or, not)
- Logical control structures (if-then, if-then-else)
- Complex nested expressions

Example:
```prolog
?- starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result)).
A is 
  findall(
    B,
    (
      member(B,[1,2,3]),
      B>1
    )
  )
```

### Output Code for a Goal

Use `starlog_output_code/1` to convert a Prolog goal to Starlog notation:

```prolog
?- starlog_output_code(string_concat("x", "y", C)).
A is "x":"y"

?- starlog_output_code(append([1,2], [3,4], L)).
A is [1,2]&[3,4]

?- starlog_output_code(reverse([1,2,3], R)).
A is reverse([1,2,3])
```

This is useful for:
- Converting existing Prolog code to Starlog notation
- Understanding how Prolog predicates map to Starlog syntax
- Generating Starlog code programmatically

### Maximal Compression

Use `starlog_output_code/3` or `starlog_output_file/3` with the `compress(true)` option to maximally compress Starlog code by nesting expressions:

```prolog
?- starlog_output_code((string_concat("hello", " ", T1), 
                        string_concat(T1, "world", T2)), _, [compress(true)]).
A is "hello":" ":"world"

?- starlog_output_code((append([1],[2],L1), reverse(L1,L2)), _, [compress(true)]).
A is reverse([1]&[2])
```

The compression algorithm:
- Nests expressions where an intermediate variable is used only once
- Preserves variables that are used multiple times
- Excludes if-then clauses, logical control structures (or, not)
- Excludes calls without an output that is another's input

Example comparison:

```prolog
% Without compression (default)
?- starlog_output_code((string_concat("a","b",T1), 
                        string_concat("c","d",T2), 
                        string_concat(T1,T2,T3)), _).
A is "a":"b",B is "c":"d",C is A:B

% With compression
?- starlog_output_code((string_concat("a","b",T1), 
                        string_concat("c","d",T2), 
                        string_concat(T1,T2,T3)), _, [compress(true)]).
A is "a":"b":("c":"d")
```

### Output Code for a File

Use `starlog_output_file/1` to convert an entire Prolog file to Starlog notation:

```prolog
?- starlog_output_file('my_program.pl').
% Starlog code output for file: my_program.pl

greet(A,B,C):-D is "Hello, ":A,E is D:" ",C is E:B.
combine_lists(A,B,C,D):-C is A&B,D is reverse(C).
...
```

Or with maximal compression using `starlog_output_file/3`:

```prolog
?- starlog_output_file('my_program.pl', user_output, [compress(true)]).
% Starlog code output for file: my_program.pl

greet(A,B,C):-C is "Hello, ":A:" ":B.
combine_lists(A,B,C,D):-D is reverse(A&B).
...
```

Or write to a file using `starlog_output_file/2` or `starlog_output_file/3`:

```prolog
?- open('output.pl', write, Stream),
   starlog_output_file('input.pl', Stream),
   close(Stream).

% With compression
?- open('output.pl', write, Stream),
   starlog_output_file('input.pl', Stream, [compress(true)]),
   close(Stream).
```

The output uses human-friendly variable names (A, B, C, ..., Z, A1, B1, ...) making the code more readable.

### Controlling eval() and no_eval() Output

By default, when outputting Starlog code, the `eval()` and `no_eval()` wrappers are **stripped** from the output, showing only their contents. This makes the output cleaner and more concise, since evaluation is the default behavior in Starlog anyway.

You can control this behavior using the `output_eval` and `output_no_eval` options:

```prolog
% Default behavior - strips both eval() and no_eval()
?- starlog_output_code(A is no_eval(1+1)).
A is 1+1

?- starlog_output_code(B is eval("x":"y")).
B is "xy"

% Keep no_eval() wrappers
?- starlog_output_code(A is no_eval(1+1), _, [output_no_eval(true)]).
A is no_eval(1+1)

% Keep eval() wrappers
?- starlog_output_code(B is eval("x":"y"), _, [output_eval(true)]).
B is eval("x":"y")

% Keep both wrappers
?- starlog_output_code(C is no_eval(eval(1+1)), _, [output_eval(true), output_no_eval(true)]).
C is no_eval(eval(1+1))
```

The same options work with `starlog_output_file/3`:

```prolog
% Default - strips both eval() and no_eval()
?- starlog_output_file('input.pl', user_output).

% Keep no_eval() wrappers
?- starlog_output_file('input.pl', user_output, [output_no_eval(true)]).

% Keep both eval() and no_eval() wrappers
?- starlog_output_file('input.pl', user_output, [output_eval(true), output_no_eval(true)]).
```

Options summary:
- `output_eval(false)` - Strip `eval()` wrappers (default)
- `output_eval(true)` - Keep `eval()` wrappers in output
- `output_no_eval(false)` - Strip `no_eval()` wrappers (default)
- `output_no_eval(true)` - Keep `no_eval()` wrappers in output

These options can be combined with `compress(true)` for maximum control over the output format.


## Converting Starlog to Prolog

The library also provides features to convert Starlog code back to standard Prolog with maximal decompression, using human-friendly variable names (A, B, C, A1, B1, etc.).

### Convert Individual Goals

Use `starlog_to_prolog_code/1` to convert a Starlog goal to standard Prolog:

```prolog
?- starlog_to_prolog_code(A is "hello":"world").
string_concat("hello","world",A)

?- starlog_to_prolog_code(A is [1,2]&[3,4]).
append([1,2],[3,4],A)

?- starlog_to_prolog_code(A is reverse([1,2]&[3,4])).
append([1,2],[3,4],A),reverse(A,B)
```

This is useful for:
- Converting Starlog code to standard Prolog
- Understanding how Starlog operators map to Prolog predicates
- Generating Prolog code from Starlog notation

### Maximal Decompression

The conversion automatically decompresses nested expressions into sequential goals:

```prolog
% Nested Starlog expression
?- starlog_to_prolog_code(A is "hello":" ":"world").
string_concat("hello"," ",A),string_concat(A,"world",B)

% Deeply nested expression
?- starlog_to_prolog_code(Result is reverse([1]&[2]&[3])).
append([1],[2],A),append(A,[3],B),reverse(B,C)
```

The decompression algorithm:
- Flattens nested expressions into sequential goals
- Maintains proper variable dependencies and execution order
- Uses human-friendly variable names (A, B, C, etc.)
- Preserves semantics of the original Starlog code

### Output Code to a Variable

```
?- starlog_to_prolog_code(A is "hello":"world", Code, [print(false)]).
Code = string_concat("hello", "world", A).
```

### Convert Entire Files

Use `starlog_to_prolog_file/1` to convert an entire Starlog file to standard Prolog:

```prolog
?- starlog_to_prolog_file('my_starlog_code.pl').
% Prolog code output for file: my_starlog_code.pl

greet(A,B):-string_concat("Hello, ",A,B).
combine_and_reverse(A,B,C):-append(A,B,D),reverse(D,C).
...
```

Or write to a file using `starlog_to_prolog_file/2`:

```prolog
?- open('output_prolog.pl', write, Stream),
   starlog_to_prolog_file('input_starlog.pl', Stream),
   close(Stream).
```

### Bidirectional Conversion

The library supports bidirectional conversion between Prolog and Starlog:

```prolog
% Prolog → Starlog (with compression)
?- starlog_output_code((string_concat("hello"," ",T1), 
                        string_concat(T1,"world",T2)), _, [compress(true)]).
A is "hello":" ":"world"

?- starlog_output_code(string_concat(A, B, C), Code).
Code = (A is B:C).

% Starlog → Prolog (with decompression)
?- starlog_to_prolog_code(A is "hello":" ":"world").
string_concat("hello"," ",A),string_concat(A,"world",B)
```

## Debugging

Enable debug output to see expansions:

```prolog
?- starlog_set_debug(true).
?- starlog_call(A is "x":"y").
Expanding goal: A is "x":"y"
Expanded to: string_concat("x","y",A)
A = "xy".
```

## Examples

### Example 1: String Processing

```prolog
:- use_module(starlog_in_prolog).

process_name(First, Last, Full) :-
    Full is First : " " : Last.

?- process_name("John", "Doe", Name).
Name = "John Doe".
```

### Example 2: List Manipulation

```prolog
:- use_module(starlog_in_prolog).

combine_and_reverse(A, B, Result) :-
    Combined is A & B,
    Result is reverse(Combined).

?- combine_and_reverse([1,2], [3,4], R).
R = [4, 3, 2, 1].
```

### Example 3: Nested Expressions

```prolog
:- use_module(starlog_in_prolog).

complex_concat(A, B, C, Result) :-
    Result is (A:B) : C.

?- complex_concat("Hello", " ", "World", R).
R = "Hello World".
```

### Example 4: Expression Preservation with no_eval

```prolog
:- use_module(starlog_in_prolog).

% Store a formula without evaluating it
store_formula(Formula) :-
    F is no_eval(Formula),
    format('Stored formula: ~w~n', [F]).

?- store_formula(x*2 + y).
Stored formula: x*2+y

% Preserve arithmetic expressions
?- A is no_eval(1+1).
A = 1+1.  % Not evaluated to 2

% Preserve Starlog operators
?- B is no_eval("hello":"world").
B = "hello":"world".  % Not concatenated
```

### Example 5: Selective Evaluation with eval inside no_eval

```prolog
:- use_module(starlog_in_prolog).

% Evaluate is the default behavior
?- A is 1+1.
A = 2.

% Explicit eval (same as default)
?- B is eval(1+1).
B = 2.

% Force evaluation inside no_eval context
?- C is no_eval(eval(1+1)).
C = 2.  % Inner expression evaluated despite no_eval

% Nested evaluation in preserved structures
?- D is no_eval("Result: " : eval("x":"y")).
D = "Result:":"xy".  % Only eval(...) part is evaluated

% Multiple eval expressions
?- E is no_eval([eval(1+1), eval(2+2), 5]).
E = [2, 4, 5].  % Only eval parts are evaluated

% Complex nested case with lists
?- F is no_eval(eval([1] & [2])).
F = [1, 2].  % List append is evaluated
```

### Example 6: Term Manipulation with Univ Operators

```prolog
:- use_module(starlog_in_prolog).

% Convert list to term
create_term(List, Term) :-
    Term is ..=(List).

?- create_term([f,0,1], T).
T = f(0,1).

% Convert term to list
term_to_list(Term, List) :-
    List is =..(Term).

?- term_to_list(foo(a,b,c), L).
L = [foo, a, b, c].

% Roundtrip conversion
?- T is ..=([bar,x,y]), L is =..(T).
T = bar(x, y),
L = [bar, x, y].
```

## Installation

1. Clone this repository
2. Load the library in your Prolog code:

```prolog
:- use_module('/path/to/starlog_in_prolog').
```

Or add it to your SWI-Prolog library path.

## Testing

Run the test suite:

```bash
cd tests
swipl -s test_basic.pl
swipl -s test_nested.pl  
swipl -s test_arithmetic_is.pl
swipl -s test_mixed_prolog_starlog.pl
swipl -s test_no_eval.pl
swipl -s test_eval.pl
```

## Requirements

* SWI-Prolog 8.x or higher

## Repository Structure

```
starlog_in_prolog/
  README.md                           # This file
  LICENSE                             # BSD-3 license
  Requirements.txt                    # Original specification
  Requirements.md                     # Additional requirements
  starlog_in_prolog.pl               # Main library module
  starlog_expand.pl                  # Expander: compile Starlog -> Prolog goals
  starlog_registry.pl                # Builtin mapping registry + extension hooks
  demo_output_feature.pl             # Demo: Prolog to Starlog conversion
  demo_starlog_to_prolog.pl          # Demo: Starlog to Prolog conversion
  tests/
    test_basic.pl                    # Basic functionality tests
    test_nested.pl                   # Nested expression tests
    test_arithmetic_is.pl            # Arithmetic preservation tests
    test_mixed_prolog_starlog.pl     # Mixed Prolog/Starlog tests
    test_starlog_to_prolog.pl        # Starlog to Prolog conversion tests
    test_starlog_to_prolog_file.pl   # File conversion tests
```

## License

BSD-3-Clause License

## Articles and Videos


## Articles and Videos

* [Example Prolog-Starlog conversions](https://lucianacademy.hashnode.dev/prolog-starlog-converter)
* [Youtube video](https://youtu.be/kdtI8VsE--4)

## Acknowledgments

Original Starlog-Prolog converter concept by luciangreenPlease. This library implements a new approach using SWI-Prolog's goal and term expansion mechanisms to allow direct entry of Starlog syntax in Prolog files.
