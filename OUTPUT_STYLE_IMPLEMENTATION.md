# Output Style Option Implementation

## Overview

This document describes the implementation of the `output_style` option for the Starlog library, which allows users to convert between nested function call syntax and method chaining syntax when outputting Starlog code.

## Problem Statement

Add `output_style(nested_calls)` or `output_style(method_chaining)` options to `starlog_to_prolog_file()` and `starlog_output_code()`.

## Solution

Implemented the `output_style` option for `starlog_output_code/3` and `starlog_output_file/3` with two transformation modes:

### 1. output_style(nested_calls)

Converts method chains (using `>>` operator) to nested function calls.

**Example:**
```prolog
Input:  X is reverse([1,2,3]) >> length
Output: X is length(reverse([1,2,3]))
```

**Usage:**
```prolog
?- starlog_output_code(
    (X is reverse([1,2,3]) >> length), 
    Code, 
    [output_style(nested_calls), print(true)]
).
% Outputs: A is length(reverse([1,2,3]))
```

### 2. output_style(method_chaining)

Converts nested function calls to method chains.

**Example:**
```prolog
Input:  X is length(reverse([1,2,3]))
Output: X is reverse([1,2,3]) >> length
```

**Usage:**
```prolog
?- starlog_output_code(
    (X is length(reverse([1,2,3]))), 
    Code, 
    [output_style(method_chaining), print(true)]
).
% Outputs: A is reverse([1,2,3])>>length
```

## Features

### Multi-Level Transformations

The implementation handles multiple levels of nesting/chaining:

```prolog
% Chain to nested
Input:  X is sort([3,1,2]) >> reverse >> length
Output: X is length(reverse(sort([3,1,2])))

% Nested to chain
Input:  X is length(reverse(sort([3,1,2])))
Output: X is sort([3,1,2]) >> reverse >> length
```

### Operator Expression Support

Works with Starlog operators (`:`, `&`, `•`):

```prolog
% List append as chain base
Input:  X is ([1,2]&[3,4]) >> reverse >> length
Output: X is length(reverse([1,2]&[3,4]))

% Extract operator base when converting to chain
Input:  X is length(reverse([1,2]&[3,4]))
Output: X is [1,2]&[3,4] >> reverse >> length
```

### Roundtrip Conversions

Transformations are reversible:

```prolog
Original: X is reverse([1,2,3]) >> length
→ Nested: X is length(reverse([1,2,3]))
→ Chain:  X is reverse([1,2,3]) >> length
```

### Integration with Other Options

The `output_style` option works seamlessly with other options:

```prolog
% With compression
?- starlog_output_code(
    (string_concat("a", "b", T1), string_concat(T1, "c", T2)),
    Code,
    [compress(true), output_style(method_chaining), print(true)]
).
% Outputs: A is "a":"b":"c"

% With variable renaming
?- starlog_output_code(
    (X is reverse([1,2,3]) >> length),
    Code,
    [output_style(nested_calls), rename(true), print(true)]
).
% Outputs: A is length(reverse([1,2,3]))
```

## Implementation Details

### Core Predicates

1. **apply_output_style/3**: Main dispatcher for style transformations
   ```prolog
   apply_output_style(+Code, +Style, -TransformedCode)
   ```

2. **transform_chains_to_nested/2**: Converts method chains to nested calls
   ```prolog
   transform_chains_to_nested(+Code, -NestedCode)
   ```

3. **transform_nested_to_chains/2**: Converts nested calls to method chains
   ```prolog
   transform_nested_to_chains(+Code, -ChainCode)
   ```

### Algorithm

#### Chain to Nested Conversion

1. Detect method chain expressions (`Base >> Method1 >> Method2 >> ...`)
2. Collect all methods in the chain into a list
3. Build nested calls from inside out: `MethodN(...Method2(Method1(Base))...)`

#### Nested to Chain Conversion

1. Detect nested function calls where the last argument is another function
2. Recursively extract the innermost expression as the base
3. Build chain from base outward: `Base >> Method1 >> Method2 >> ...`
4. Handle operator expressions as chain bases

### Edge Cases Handled

- Single function calls (no transformation needed)
- Simple values without functions (pass through)
- Operator expressions as function arguments
- Mixed nested and operator expressions
- Functions with multiple arguments

## API

### starlog_output_code/3

```prolog
starlog_output_code(+Goal, -StarlogCode, +Options)
```

**Options:**
- `output_style(nested_calls)` - Convert method chains to nested calls
- `output_style(method_chaining)` - Convert nested calls to method chains
- Other options: `compress(true/false)`, `print(true/false)`, `rename(true/false)`, etc.

### starlog_output_file/3

```prolog
starlog_output_file(+FilePath, +OutputStream, +Options)
```

**Options:**
- `output_style(nested_calls)` - Convert method chains to nested calls
- `output_style(method_chaining)` - Convert nested calls to method chains
- Other options: `compress(true/false)`, etc.

## Testing

### Test Suite

Created comprehensive test suite in `tests/test_output_style.pl`:
- 19 tests covering all transformation scenarios
- All tests passing
- Tests include:
  - Basic transformations
  - Multi-level transformations
  - Operator expressions
  - Edge cases
  - Idempotence tests
  - Complex combinations

### Demonstration

Created demonstration file `demo_output_style.pl` showing:
- Basic transformations
- Multi-level transformations
- Integration with Starlog operators
- Roundtrip conversions
- Combination with other options
- File operations

## Examples

### Example 1: Basic Usage

```prolog
?- use_module(starlog).

% Convert chain to nested
?- starlog_output_code(
    (X is reverse([1,2,3]) >> length),
    Code,
    [output_style(nested_calls)]
).
Code = (_ is length(reverse([1,2,3]))).

% Convert nested to chain
?- starlog_output_code(
    (X is length(reverse([1,2,3]))),
    Code,
    [output_style(method_chaining)]
).
Code = (_ is reverse([1,2,3])>>length).
```

### Example 2: File Processing

```prolog
% Convert a Starlog file to show code in nested style
?- starlog_output_file(
    'my_code.pl',
    user_output,
    [output_style(nested_calls)]
).

% Convert a Starlog file to show code in chaining style
?- starlog_output_file(
    'my_code.pl',
    user_output,
    [output_style(method_chaining)]
).
```

### Example 3: With Compression

```prolog
% Compress and convert to chain style
?- starlog_output_code(
    (string_concat("a", "b", T1), string_concat(T1, "c", T2)),
    Code,
    [compress(true), output_style(method_chaining), print(true)]
).
% Outputs: A is "a":"b":"c"
```

## Benefits

1. **Readability**: Choose the style that's most readable for your use case
2. **Flexibility**: Convert between styles as needed
3. **Compatibility**: Works with all existing Starlog features
4. **Reversibility**: Transformations preserve semantics
5. **Integration**: Seamless integration with other options

## Files Modified

1. **starlog.pl**
   - Added `apply_output_style/3` predicate
   - Added transformation predicates
   - Updated `starlog_output_code/3`
   - Updated `starlog_output_file/3`
   - Added helper predicates for conversion

2. **tests/test_output_style.pl** (new)
   - Comprehensive test suite
   - 19 tests covering all scenarios

3. **demo_output_style.pl** (new)
   - Comprehensive demonstration
   - Shows all features and use cases

## Backward Compatibility

All changes are backward compatible:
- Existing code works without modification
- The `output_style` option is optional
- Default behavior unchanged when option not specified
- All existing tests pass

## Future Enhancements

Potential future improvements:
1. Support for custom style preferences
2. Automatic style detection
3. Style consistency checking
4. Integration with IDE/editor tools
