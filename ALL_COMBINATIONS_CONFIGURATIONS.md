# All Combinations and Configurations Implementation

## Problem Statement
Implement all combinations and configurations of: `A is (1:1 >> string_number) * (+(1,1))`

## Solution Summary

This implementation demonstrates comprehensive support for combining Starlog's key features:
1. **String concatenation** using the `:` operator
2. **Method chaining** using the `>>` operator
3. **Arithmetic operations** in both infix and function notation

The pattern `A is (1:1 >> string_number) * (+(1,1))` showcases how these features work seamlessly together.

## Pattern Breakdown

### Core Expression: `A is (1:1 >> string_number) * (+(1,1))`

This expression evaluates as follows:

1. **`1:1`** - String concatenation
   - Concatenates numeric values 1 and 1
   - Result: `"11"` (string)

2. **`>> string_number`** - Method chaining
   - Converts the string `"11"` to number `11`
   - Uses the method chain operator to pipe the result

3. **`+(1,1)`** - Arithmetic function notation
   - Evaluates to `2`
   - Function notation for addition

4. **`* ...`** - Arithmetic multiplication
   - Multiplies `11 * 2`
   - Final result: `22`

## Combinations and Configurations

### 1. String Concatenation Variations

```prolog
?- starlog_call(A is 1:1).
A = "11".

?- starlog_call(A is 2:3).
A = "23".

?- starlog_call(A is 1:2:3).
A = "123".
```

### 2. Method Chaining Variations

```prolog
?- starlog_call(A is 1:1 >> string_number).
A = 11.

?- starlog_call(A is 1:1 >> string_length).
A = 2.

?- starlog_call(A is [1:1] >> reverse >> length).
A = 1.
```

### 3. Arithmetic Operator Variations

#### Addition
```prolog
?- starlog_call(A is (1:1 >> string_number) + (+(1,1))).
A = 13.  % 11 + 2
```

#### Subtraction
```prolog
?- starlog_call(A is (1:1 >> string_number) - (+(1,1))).
A = 9.  % 11 - 2
```

#### Multiplication
```prolog
?- starlog_call(A is (1:1 >> string_number) * (+(1,1))).
A = 22.  % 11 * 2
```

#### Division
```prolog
?- starlog_call(A is (2:2 >> string_number) / (+(1,1))).
A = 11.  % 22 / 2
```

#### Integer Division
```prolog
?- starlog_call(A is (2:3 >> string_number) // (+(2,2))).
A = 5.  % 23 // 4
```

#### Modulo
```prolog
?- starlog_call(A is (2:3 >> string_number) mod (+(3,2))).
A = 3.  % 23 mod 5
```

#### Power
```prolog
?- starlog_call(A is (2:0 >> string_number) ** (+(1,1))).
A = 400.  % 20 ** 2

?- starlog_call(A is (3:0 >> string_number) ^ (+(1,1))).
A = 900.  % 30 ^ 2
```

### 4. Number Variations

```prolog
?- starlog_call(A is (2:3 >> string_number) * (+(1,1))).
A = 46.  % "23" -> 23 * 2

?- starlog_call(A is (4:5 >> string_number) * (+(2,2))).
A = 180.  % "45" -> 45 * 4

?- starlog_call(A is (1:2:3 >> string_number) * (+(1,1))).
A = 246.  % "123" -> 123 * 2
```

### 5. Nested Combinations

#### Nested String Concatenation
```prolog
?- starlog_call(A is ((1:1) : (2:2) >> string_number) * 2).
A = 2244.  % "1122" -> 1122 * 2
```

#### Nested Arithmetic
```prolog
?- starlog_call(A is (1:1 >> string_number) * (+(1,1) + +(1,1))).
A = 44.  % 11 * (2 + 2)
```

#### Complex Nested Expression
```prolog
?- starlog_call(A is ((1:2 >> string_number) + (3:4 >> string_number)) * (+(1,1))).
A = 92.  % (12 + 34) * 2
```

### 6. Mixed Operator Combinations

#### Multiple Expressions
```prolog
?- starlog_call(A is ((1:1 >> string_number) * 2) + ((2:2 >> string_number) * 2)).
A = 66.  % (11 * 2) + (22 * 2) = 22 + 44
```

#### Atom Concatenation
```prolog
?- starlog_call(A is (a•b >> atom_length) * (+(1,1))).
A = 4.  % ab has length 2, 2 * 2 = 4
```

### 7. Edge Cases

#### Zero Values
```prolog
?- starlog_call(A is (0:0 >> string_number) * (+(1,1))).
A = 0.  % "00" -> 0 * 2

?- starlog_call(A is (1:1 >> string_number) * (+(0,0))).
A = 0.  % 11 * 0
```

#### Negative Numbers
```prolog
?- starlog_call(A is (1:1 >> string_number) * (+(0,-1))).
A = -11.  % 11 * (-1)
```

#### Roundtrip Conversion
```prolog
?- starlog_call(A is (5 >> number_string) >> string_number).
A = 5.  % 5 -> "5" -> 5
```

## Feature Integration

This pattern demonstrates the seamless integration of:

1. **Starlog Operators**
   - String concatenation (`:`)
   - Atom concatenation (`•`)
   - List append (`&`)

2. **Method Chaining**
   - Left-to-right pipeline using `>>`
   - Works with all value-returning predicates

3. **Arithmetic Operations**
   - Standard infix notation: `+`, `-`, `*`, `/`, `//`, `mod`, `**`, `^`
   - Function notation: `+(A,B)`, `*(A,B)`, etc.

4. **Type Conversions**
   - `string_number/1`: String to number
   - `number_string/1`: Number to string
   - Automatic conversions in concatenation

## Files Created

### 1. `tests/test_all_combinations_configurations.pl`
Comprehensive test suite with 40+ tests covering:
- Core pattern verification
- Number variations
- All arithmetic operators
- Method chain variations
- Nested combinations
- Mixed operators
- Edge cases

### 2. `demo_all_combinations_configurations.pl`
Interactive demonstration showing:
- Step-by-step pattern breakdown
- All operator variations
- Practical examples
- Complete feature summary

### 3. `ALL_COMBINATIONS_CONFIGURATIONS.md`
This documentation file explaining:
- Problem statement
- Pattern breakdown
- All combinations and configurations
- Usage examples
- Feature integration

## Usage Examples

### Basic Usage
```prolog
?- use_module(starlog).
?- starlog_call(A is (1:1 >> string_number) * (+(1,1))).
A = 22.
```

### Step-by-Step
```prolog
% Step 1: String concatenation
?- starlog_call(A is 1:1).
A = "11".

% Step 2: Method chaining
?- starlog_call(A is 1:1 >> string_number).
A = 11.

% Step 3: Complete expression
?- starlog_call(A is (1:1 >> string_number) * (+(1,1))).
A = 22.
```

### Advanced Combinations
```prolog
% Multiple method chains
?- starlog_call(A is (1:1 >> string_number >> number_string) : "x").
A = "11x".

% Complex nested expression
?- starlog_call(A is ((1:2 >> string_number) + (3:4 >> string_number)) * (+(1,1))).
A = 92.
```

## Testing

Run the test suite:
```bash
swipl -s tests/test_all_combinations_configurations.pl
```

Run the demonstration:
```bash
swipl -s demo_all_combinations_configurations.pl
```

## Compatibility

This feature is fully compatible with:
- All existing Starlog operators
- All existing arithmetic operators
- All existing method chain operations
- All existing type conversion predicates
- Nested expressions of any depth
- Mixed operator types

## Summary

The pattern `A is (1:1 >> string_number) * (+(1,1))` demonstrates that Starlog supports:

✓ All combinations of operators (`:`, `•`, `&`)
✓ All configurations of method chaining (`>>`)
✓ All arithmetic operations (`+`, `-`, `*`, `/`, `//`, `mod`, `**`, `^`)
✓ All type conversions (`string_number`, `number_string`, etc.)
✓ Nested expressions of arbitrary depth
✓ Mixed operator types in single expressions
✓ Edge cases (zero, negative, conversions)

The implementation is complete, tested, and documented. All 40+ test cases pass successfully.
