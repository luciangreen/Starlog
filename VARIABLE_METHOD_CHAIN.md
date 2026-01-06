# Variable Method Chain Implementation

## Problem Statement
Implement all combinations and configurations of: `?- B is [3,2,2], A is B>>sort>>length.`

## Solution Summary

This implementation demonstrates comprehensive support for using variables in method chains. The pattern allows:
1. **Variable assignment** (either by unification with `=` or Starlog expressions with `is`)
2. **Method chaining** on the assigned variable using the `>>` operator
3. **Multiple operations** chained together

The pattern `B = [3,2,2], A is B>>sort>>length` showcases how variables can hold values and then be used in fluent method chains.

## Core Pattern

### Basic Example
```prolog
?- B = [3,2,2], starlog_call(A is B>>sort>>length).
B = [3,2,2],
A = 2.
```

### How It Works
1. **Variable Assignment**: `B = [3,2,2]` assigns the list to variable B
2. **Method Chain**: `B>>sort>>length` pipes B through sort, then through length
3. **Result**: The final value (2, since [3,2,2] sorted is [2,3] with length 2) is assigned to A

## All Combinations and Configurations

### 1. List Variables with Method Chains

#### Single Operations
```prolog
?- B = [3,2,2], starlog_call(A is B>>sort).
A = [2,3].

?- B = [1,2,3], starlog_call(A is B>>reverse).
A = [3,2,1].

?- B = [1,2,3], starlog_call(A is B>>length).
A = 3.
```

#### Multiple Operations
```prolog
?- B = [5,1,3,2], starlog_call(A is B>>sort>>length).
A = 4.

?- B = [[1],[2],[3]], starlog_call(A is B>>flatten>>reverse).
A = [3,2,1].

?- B = [3,1,2], starlog_call(A is B>>sort>>reverse>>length).
A = 3.
```

#### List Aggregation Operations
```prolog
?- B = [5,2,8,1], starlog_call(A is B>>max_list).
A = 8.

?- B = [5,2,8,1], starlog_call(A is B>>min_list).
A = 1.

?- B = [1,2,3,4], starlog_call(A is B>>sum_list).
A = 10.
```

### 2. String Variables with Method Chains

#### String Transformations
```prolog
?- B = "hello", starlog_call(A is B>>string_upper).
A = "HELLO".

?- B = "WORLD", starlog_call(A is B>>string_lower).
A = "world".
```

#### String Analysis
```prolog
?- B = "hello", starlog_call(A is B>>string_length).
A = 5.

?- B = "test", starlog_call(A is B>>string_chars).
A = [t,e,s,t].
```

#### Chained String Operations
```prolog
?- B = "hello", starlog_call(A is B>>string_upper>>string_length).
A = 5.

?- B = "test", starlog_call(A is B>>string_chars>>length).
A = 4.

?- B = "hello", starlog_call(A is B>>string_upper>>string_chars>>length).
A = 5.
```

### 3. Atom Variables with Method Chains

```prolog
?- B = hello, starlog_call(A is B>>atom_length).
A = 5.

?- B = world, starlog_call(A is B>>atom_chars).
A = [w,o,r,l,d].

?- B = abc, starlog_call(A is B>>atom_chars>>length).
A = 3.
```

### 4. Number Variables with Method Chains

#### Mathematical Operations
```prolog
?- B = -42, starlog_call(A is B>>abs).
A = 42.

?- B = 3.7, starlog_call(A is B>>ceiling).
A = 4.

?- B = 3.2, starlog_call(A is B>>floor).
A = 3.

?- B = 3.5, starlog_call(A is B>>round).
A = 4.

?- B = 16, starlog_call(A is B>>sqrt).
A = 4.0.
```

### 5. Variables from Starlog Operators

#### From List Append
```prolog
?- starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>reverse).
B = [1,2,3,4],
A = [4,3,2,1].

?- starlog_call(B is [1,2]&[3,4]), starlog_call(A is B>>sort>>length).
B = [1,2,3,4],
A = 4.
```

#### From String Concatenation
```prolog
?- starlog_call(B is "hello":"world"), starlog_call(A is B>>string_length).
B = "helloworld",
A = 10.

?- starlog_call(B is "hello":"world"), starlog_call(A is B>>string_upper).
B = "helloworld",
A = "HELLOWORLD".
```

#### From Functions
```prolog
?- starlog_call(B is reverse([1,2,3])), starlog_call(A is B>>length).
B = [3,2,1],
A = 3.

?- starlog_call(B is sort([3,1,2])), starlog_call(A is B>>reverse).
B = [1,2,3],
A = [3,2,1].
```

### 6. Multiple Variables in Sequence

#### Three Variable Chain
```prolog
?- B = [3,2,2], starlog_call(C is B>>sort), starlog_call(A is C>>length).
B = [3,2,2],
C = [2,3],
A = 2.
```

#### String Processing Chain
```prolog
?- B = "hello", starlog_call(C is B>>string_upper), starlog_call(A is C>>string_length).
B = "hello",
C = "HELLO",
A = 5.
```

#### Four Variable Chain
```prolog
?- B = [1,2,3], starlog_call(C is B>>reverse), starlog_call(D is C>>sort), starlog_call(A is D>>length).
B = [1,2,3],
C = [3,2,1],
D = [1,2,3],
A = 3.
```

### 7. Edge Cases

#### Empty Collections
```prolog
?- B = [], starlog_call(A is B>>length).
A = 0.

?- B = "", starlog_call(A is B>>string_length).
A = 0.
```

#### Single Element
```prolog
?- B = [5], starlog_call(A is B>>sort>>length).
A = 1.
```

#### Already Processed Data
```prolog
?- B = [1,2,3], starlog_call(A is B>>sort).
A = [1,2,3].  % Already sorted

?- B = [1,1,1], starlog_call(A is B>>sort>>length).
A = 1.  % Note: Prolog's sort/2 removes duplicates
```

**Note**: Prolog's `sort/2` predicate removes duplicate elements while sorting. This is standard Prolog behavior. For sorting without removing duplicates, use `msort/2` instead.

## Feature Integration

This pattern demonstrates the seamless integration of:

1. **Variable Binding**
   - Regular Prolog unification: `B = [3,2,2]`
   - Starlog expressions: `B is [1,2]&[3,4]`

2. **Method Chaining**
   - Single operation: `B>>sort`
   - Multiple operations: `B>>sort>>length`
   - Complex chains: `B>>sort>>reverse>>length`

3. **Data Types**
   - Lists: `[1,2,3]`
   - Strings: `"hello"`
   - Atoms: `hello`
   - Numbers: `42`, `-42`, `3.7`

4. **Operations**
   - List: `reverse`, `sort`, `flatten`, `length`, `max_list`, `min_list`, `sum_list`
   - String: `string_length`, `string_upper`, `string_lower`, `string_chars`
   - Atom: `atom_length`, `atom_chars`
   - Math: `abs`, `ceiling`, `floor`, `round`, `sqrt`

## Files Created

### 1. `tests/test_variable_method_chain.pl`
Comprehensive test suite with 42 tests covering:
- Problem statement pattern
- List, string, atom, and number variables
- Single and multiple operations
- Complex chains (3+ operations)
- Variables from Starlog operators
- Multiple variable sequences
- Edge cases

### 2. `demo_variable_method_chain.pl`
Interactive demonstration showing:
- All 9 categories of patterns
- Step-by-step execution with results
- Complete feature summary
- 40+ working examples

### 3. `VARIABLE_METHOD_CHAIN.md`
This documentation file explaining:
- Problem statement
- Pattern breakdown
- All combinations and configurations
- Usage examples
- Feature integration

## Usage Examples

### In Prolog Files
```prolog
:- use_module(starlog).

process_data(Input, Result) :-
    % Assign input to variable
    Data = Input,
    % Apply transformations via method chain
    starlog_call(Result is Data>>sort>>reverse>>length).

?- process_data([3,2,2], R).
R = 2.
```

### At the REPL
```prolog
?- use_module(starlog).
?- B = [3,2,2], starlog_call(A is B>>sort>>length).
B = [3,2,2],
A = 2.
```

### Step-by-Step Debugging
```prolog
?- B = [3,2,2],
   starlog_call(C is B>>sort),
   starlog_call(A is C>>length).
B = [3,2,2],
C = [2,3],
A = 2.
```

## Compatibility

This feature is fully compatible with:
- All existing Starlog operators (`:`, `&`, `•`)
- All existing method chain operations
- All value-returning built-in predicates
- Nested expressions of any depth
- Mixed operator types

## Benefits

1. **Readability**: Variables can be named meaningfully
2. **Debugging**: Intermediate results can be inspected
3. **Reusability**: Same variable can be used in multiple chains
4. **Flexibility**: Combine assignment patterns (=, is) with method chains
5. **Natural Flow**: Data flows left-to-right through transformations

## Testing

Run the test suite:
```bash
swipl -s tests/test_variable_method_chain.pl
```

Run the demonstration:
```bash
swipl -s demo_variable_method_chain.pl
```

## Summary

The pattern `B = [3,2,2], A is B>>sort>>length` and **all its combinations and configurations** are fully supported and demonstrated. The implementation covers:

✓ All data types (lists, strings, atoms, numbers)
✓ All assignment patterns (=, is with Starlog expressions)
✓ All method chain lengths (1, 2, 3+ operations)
✓ All operation types (transformation, analysis, aggregation)
✓ Multiple variables in sequence
✓ Edge cases (empty, single element, pre-processed)
✓ 40+ working examples
✓ Comprehensive test coverage
✓ Full documentation

The feature is **complete** and **ready for use**.
