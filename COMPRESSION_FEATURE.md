# Maximal Compression Feature

## Overview

The maximal compression feature adds the ability to nest Starlog expressions when converting from Prolog to Starlog notation. This creates more concise, readable code by eliminating unnecessary intermediate variables.

## Usage

### Basic Usage

```prolog
% With compression
?- starlog_output_code((string_concat("hello"," ",T1), string_concat(T1,"world",T2)), _, [compress(true)]).
A is "hello":" ":"world"

% Without compression (default)
?- starlog_output_code((string_concat("hello"," ",T1), string_concat(T1,"world",T2)), _).
A is "hello":" ",B is A:"world"
```

### File Compression

```prolog
% Compress entire file
?- starlog_output_file('input.pl', user_output, [compress(true)]).

% Write compressed output to file
?- open('output.pl', write, Stream),
   starlog_output_file('input.pl', Stream, [compress(true)]),
   close(Stream).
```

## Algorithm

The compression algorithm:

1. **Identifies nestable expressions**: Variables that are used exactly once as output and once as input
2. **Nests expressions**: Replaces the variable with the expression that produces it
3. **Preserves multi-use variables**: Variables used multiple times remain as separate assignments
4. **Excludes control structures**: Does not nest if-then, or, not, or other control structures
5. **Iterates until stable**: Applies nesting repeatedly until no more compression is possible

## Examples

### Example 1: String Concatenation

```prolog
% Prolog input
greet(First, Last, Greeting) :-
    string_concat("Hello, ", First, Temp),
    string_concat(Temp, " ", Temp2),
    string_concat(Temp2, Last, Greeting).

% Compressed Starlog output
greet(A,B,C):-C is "Hello, ":A:" ":B.
```

### Example 2: List Operations

```prolog
% Prolog input
combine_and_reverse(A, B, Result) :-
    append(A, B, Combined),
    reverse(Combined, Result).

% Compressed Starlog output
combine_and_reverse(A,B,C):-C is reverse(A&B).
```

### Example 3: Multiple Uses (No Nesting)

```prolog
% Prolog input
process(X, Y, Z) :-
    string_concat("Hello", X, Temp),
    string_concat(Temp, "world", Y),
    string_concat(Temp, "friend", Z).

% Compressed Starlog output (Temp used twice, so not nested)
process(A,B,C):-D is "Hello":A,B is D:"world",C is D:"friend".
```

### Example 4: Complex Nesting

```prolog
% Prolog input
complex(A, B, C, Result) :-
    string_concat(A, B, AB),
    string_concat(AB, C, ABC),
    string_length(ABC, Len),
    atom_concat(result, Len, Result).

% Compressed Starlog output
complex(A,B,C,D):-D is result•string_length(A:B:C).
```

## API

### starlog_output_code/3

```prolog
starlog_output_code(+Goal, -StarlogCode, +Options)
```

Options:
- `compress(true)` - Apply maximal compression
- `compress(false)` - No compression (default)

### starlog_output_file/3

```prolog
starlog_output_file(+FilePath, +OutputStream, +Options)
```

Options:
- `compress(true)` - Apply maximal compression
- `compress(false)` - No compression (default)

## Implementation Details

### Key Predicates

- `compress_starlog/2` - Main compression entry point
- `compress_goals_iterative/2` - Iteratively applies compression
- `try_nest_goal/3` - Attempts to nest a single goal
- `count_var_uses/3` - Counts variable usage
- `substitute_term/4` - Replaces variables in terms

### Exclusions

The compression algorithm excludes:
- If-then clauses: `(Cond -> Then)`
- If-then-else: `(Cond -> Then ; Else)`
- Disjunction: `(A ; B)`
- Negation: `\+ Goal`, `not(Goal)`
- Cut: `!`
- Variables used multiple times
- Circular references

## Testing

Comprehensive tests are included in:
- `tests/test_compression.pl` - Goal-level compression tests
- `tests/test_file_compression.pl` - File-level compression tests

Run tests:
```bash
cd tests
swipl -s test_compression.pl
swipl -s test_file_compression.pl
```

## Benefits

1. **More concise code**: Eliminates unnecessary intermediate variables
2. **Better readability**: Nested expressions are often easier to understand
3. **Functional style**: Makes Starlog look more like functional programming
4. **Backward compatible**: Default behavior unchanged; opt-in feature

## Future Enhancements

Potential improvements:
- Configurable nesting depth limit
- Heuristics for when to nest vs. when to keep variables
- Support for user-defined nesting rules
- Pretty-printing with line wrapping for long nested expressions
