# Starlog to Prolog Conversion Feature

## Overview

The Starlog to Prolog conversion feature allows you to convert Starlog syntax back to standard Prolog with maximal decompression. This is the inverse of the Prolog to Starlog converter, and together they provide bidirectional conversion between the two notations.

## Key Features

1. **Maximal Decompression**: Nested Starlog expressions are flattened into sequential Prolog goals
2. **Human-Friendly Variable Names**: Uses A, B, C, ..., Z, A1, B1, ... naming scheme
3. **Operator Translation**: Converts Starlog operators (`:`, `&`, `•`) to standard Prolog predicates
4. **Preserves Semantics**: The converted Prolog code is semantically equivalent to the original Starlog code

## Usage

### Converting Individual Goals

Use `starlog_to_prolog_code/1` to convert a single Starlog goal:

```prolog
?- starlog_to_prolog_code(A is "hello":"world").
string_concat("hello","world",A)

?- starlog_to_prolog_code(A is [1,2]&[3,4]).
append([1,2],[3,4],A)

?- starlog_to_prolog_code(A is hello•world).
atom_concat(hello,world,A)
```

### Decompressing Nested Expressions

The converter automatically decompresses nested expressions:

```prolog
% Simple nesting
?- starlog_to_prolog_code(A is reverse([1,2]&[3,4])).
append([1,2],[3,4],A),reverse(A,B)

% Multiple levels of nesting
?- starlog_to_prolog_code(A is "hello":" ":"world").
string_concat("hello"," ",A),string_concat(A,"world",B)

% Complex nested expression
?- starlog_to_prolog_code(Result is reverse([1]&[2]&[3])).
append([1],[2],A),append(A,[3],B),reverse(B,C)
```

### Converting Files

Use `starlog_to_prolog_file/1` to convert an entire file:

```prolog
?- starlog_to_prolog_file('my_starlog_code.pl').
% Prolog code output for file: my_starlog_code.pl

greet(A,B):-string_concat("Hello, ",A,B).
combine_and_reverse(A,B,C):-append(A,B,D),reverse(D,C).
process_text(A,B,C):-string_concat(A," ",D),string_concat(D,B,C).
...
```

Write to a file using `starlog_to_prolog_file/2`:

```prolog
?- open('output.pl', write, Stream),
   starlog_to_prolog_file('input_starlog.pl', Stream),
   close(Stream).
```

## API

### starlog_to_prolog_code/1

```prolog
starlog_to_prolog_code(+StarlogGoal)
```

Convert a Starlog goal to Prolog and output it.

**Parameters:**
- `StarlogGoal`: A Starlog expression or goal to convert

**Example:**
```prolog
?- starlog_to_prolog_code(A is "x":"y").
string_concat("x","y",A)
```

### starlog_to_prolog_code/2

```prolog
starlog_to_prolog_code(+StarlogGoal, -PrologCode)
```

Convert a Starlog goal to Prolog and return it as a term.

**Parameters:**
- `StarlogGoal`: A Starlog expression or goal to convert
- `PrologCode`: The resulting Prolog code as a term

**Example:**
```prolog
?- starlog_to_prolog_code(A is "x":"y", Code).
Code = string_concat("x","y",A).
```

### starlog_to_prolog_code/3

```prolog
starlog_to_prolog_code(+StarlogGoal, -PrologCode, +Options)
```

Convert a Starlog goal to Prolog with options.

**Parameters:**
- `StarlogGoal`: A Starlog expression or goal to convert
- `PrologCode`: The resulting Prolog code as a term
- `Options`: List of options (currently supports `decompress(true/false)`, default is `true`)

**Example:**
```prolog
?- starlog_to_prolog_code(A is "x":"y", Code, [decompress(true)]).
Code = string_concat("x","y",A).
```

### starlog_to_prolog_file/1

```prolog
starlog_to_prolog_file(+FilePath)
```

Convert a Starlog file to Prolog and output to stdout.

**Parameters:**
- `FilePath`: Path to the Starlog file to convert

### starlog_to_prolog_file/2

```prolog
starlog_to_prolog_file(+FilePath, +OutputStream)
```

Convert a Starlog file to Prolog and write to a stream.

**Parameters:**
- `FilePath`: Path to the Starlog file to convert
- `OutputStream`: Output stream to write the Prolog code to

### starlog_to_prolog_file/3

```prolog
starlog_to_prolog_file(+FilePath, +OutputStream, +Options)
```

Convert a Starlog file to Prolog with options.

**Parameters:**
- `FilePath`: Path to the Starlog file to convert
- `OutputStream`: Output stream to write the Prolog code to
- `Options`: List of options (currently supports `decompress(true/false)`, default is `true`)

## Decompression Algorithm

The decompression algorithm works as follows:

1. **Expansion**: Uses the existing `expand_starlog_goal/2` predicate from `starlog_expand.pl` to expand Starlog expressions into Prolog goals
2. **Flattening**: The expansion process automatically flattens nested expressions:
   - `A is reverse([1,2]&[3,4])` expands to two goals: `append([1,2],[3,4],Temp), reverse(Temp,A)`
   - `A is "hello":" ":"world"` expands to two goals: `string_concat("hello"," ",Temp1), string_concat(Temp1,"world",A)`
3. **Variable Renaming**: Applies human-friendly variable names (A, B, C, ..., Z, A1, B1, ...) to all variables

## Operator Translation

The converter handles the following Starlog operators:

| Starlog Operator | Prolog Predicate | Description |
|-----------------|------------------|-------------|
| `:` | `string_concat/3` | String concatenation |
| `&` | `append/3` | List append |
| `•` | `atom_concat/3` | Atom concatenation |

Additionally, it converts value-returning builtins:

```prolog
% Starlog: A is reverse([1,2,3])
% Prolog:  reverse([1,2,3],A)

% Starlog: A is string_length("hello")
% Prolog:  string_length("hello",A)
```

## Variable Naming Scheme

The converter uses human-friendly variable names following this pattern:

- First 26 variables: A, B, C, ..., Z
- Next 26 variables: A1, B1, C1, ..., Z1
- Next 26 variables: A2, B2, C2, ..., Z2
- And so on...

This makes the converted code more readable than using generated variable names like `_G123`, `_G456`, etc.

## Examples

### Example 1: Simple String Concatenation

```prolog
% Starlog input
greet(Name, Greeting) :-
    Greeting is "Hello, ":Name.

% Prolog output
greet(A,B):-string_concat("Hello, ",A,B).
```

### Example 2: List Operations with Nesting

```prolog
% Starlog input
combine_and_reverse(A, B, Result) :-
    Result is reverse(A&B).

% Prolog output (decompressed)
combine_and_reverse(A,B,C):-append(A,B,D),reverse(D,C).
```

### Example 3: Multiple Nested Operations

```prolog
% Starlog input
process_text(First, Last, Full) :-
    Full is First:" ":Last.

% Prolog output (decompressed)
process_text(A,B,C):-string_concat(A," ",D),string_concat(D,B,C).
```

### Example 4: Complex Nested Expression

```prolog
% Starlog input
complex_concat(A, B, C, Result) :-
    Result is (A:B):C.

% Prolog output (decompressed)
complex_concat(A,B,C,D):-string_concat(A,B,E),string_concat(E,C,D).
```

## Bidirectional Conversion

The library supports bidirectional conversion:

```prolog
% Start with Prolog
Prolog = (string_concat("hello"," ",T1), string_concat(T1,"world",T2))

% Convert to compressed Starlog
?- starlog_output_code(Prolog, Starlog, [compress(true)]).
Starlog = (A is "hello":" ":"world")

% Convert back to decompressed Prolog
?- starlog_to_prolog_code(Starlog, BackToProlog).
BackToProlog = (string_concat("hello"," ",A), string_concat(A,"world",B))
```

## Testing

Run the test suite to verify the conversion:

```bash
cd tests
swipl -s test_starlog_to_prolog.pl
swipl -s test_starlog_to_prolog_file.pl
```

## Demo

Run the comprehensive demo:

```bash
swipl -s demo_starlog_to_prolog.pl
```

This will show various examples of Starlog to Prolog conversion with maximal decompression.

## Implementation Details

The conversion is implemented in `starlog_in_prolog.pl` using:

- **`starlog_expand:expand_starlog_goal/2`**: Expands Starlog expressions to Prolog goals (handles decompression)
- **`rename_variables/2`**: Applies human-friendly variable names
- **`write_term/3`**: Outputs the result with proper formatting

The decompression is automatic because the `expand_starlog_goal/2` predicate already flattens nested expressions when expanding them. This ensures that nested Starlog expressions like `A is reverse([1,2]&[3,4])` are properly decomposed into sequential goals.

## Benefits

1. **Readability**: Human-friendly variable names make the code easier to understand
2. **Portability**: Converted code can be used in any Prolog system
3. **Debugging**: Easier to debug Prolog code than nested Starlog expressions
4. **Education**: Helps understand how Starlog translates to standard Prolog
5. **Interoperability**: Allows Starlog code to be used with existing Prolog tools and libraries
