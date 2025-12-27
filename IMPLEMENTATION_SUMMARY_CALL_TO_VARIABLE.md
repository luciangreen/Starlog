# Implementation Summary: Save Calls to Variables

**Date**: 2025-12-26  
**Feature**: Save Prolog and Starlog calls to variables

## Overview

This feature adds predicates to explicitly save the results of Prolog and Starlog calls to variables. While variables already work through Prolog's unification mechanism, these new predicates make it more explicit and provide convenient helpers for working with `eval()` and `no_eval()`.

## New Predicates

### 1. starlog_call/2

**Signature**: `starlog_call(+Goal, -Result)`

**Purpose**: Execute a Starlog goal and explicitly return the result in a variable.

**Usage**:
```prolog
?- starlog_call(X is "hello":"world", Result).
Result = "helloworld".

?- starlog_call(Y is [1,2] & [3,4], Result).
Result = [1, 2, 3, 4].

?- starlog_call(Z is reverse([1,2,3]), Result).
Result = [3, 2, 1].
```

**Implementation**:
- Accepts goals of the form `Var is Expr`
- Expands the Starlog goal using `expand_starlog_goal/2`
- Executes the expanded goal
- Returns the result by unifying with the variable

**Error handling**: Throws a type_error if the goal is not of the form `Var is Expr`

### 2. starlog_eval/2

**Signature**: `starlog_eval(+Expr, -Result)`

**Purpose**: Evaluate a Starlog expression and return the result. This forces evaluation of the expression, similar to wrapping it in `eval()`.

**Usage**:
```prolog
?- starlog_eval("x":"y", Result).
Result = "xy".

?- starlog_eval(1+1, Result).
Result = 2.

?- starlog_eval([a] & [b,c], Result).
Result = [a, b, c].
```

**Implementation**:
- Internally calls `starlog_call((Result is eval(Expr)), Result)`
- The `eval()` wrapper ensures the expression is evaluated

### 3. starlog_no_eval/2

**Signature**: `starlog_no_eval(+Expr, -Result)`

**Purpose**: Preserve a Starlog expression without evaluation and return it. This prevents evaluation of the expression, similar to wrapping it in `no_eval()`.

**Usage**:
```prolog
?- starlog_no_eval(1+1, Result).
Result = 1+1.

?- starlog_no_eval("hello":"world", Result).
Result = "hello":"world".

?- starlog_no_eval([a] & [b], Result).
Result = [a] & [b].
```

**Implementation**:
- Internally calls `starlog_call((Result is no_eval(Expr)), Result)`
- The `no_eval()` wrapper ensures the expression is preserved as-is

## Use Cases

1. **Explicit result capture**: When you want to make it clear that you're saving a result to a variable

2. **Functional composition**: Building pipelines of operations where each step's result is saved

3. **Symbolic computation**: Using `starlog_no_eval/2` to build expressions as data structures

4. **Template systems**: Storing unevaluated expressions for later substitution

5. **Meta-programming**: Manipulating code as data

## Examples

### Example 1: Chaining Operations
```prolog
?- starlog_call(A is "First":"Part", R1),
   starlog_call(B is R1:" Complete", R2).
R1 = "FirstPart",
R2 = "FirstPart Complete".
```

### Example 2: Building Formulas
```prolog
?- starlog_no_eval(x*2 + y, Formula).
Formula = x*2+y.
% Formula can now be used in symbolic computation
```

### Example 3: Comparison
```prolog
?- starlog_eval("a":"b", Evaluated),
   starlog_no_eval("a":"b", Preserved).
Evaluated = "ab",
Preserved = "a":"b".
```

### Example 4: Template System
```prolog
% Store template
?- starlog_no_eval("Welcome, " : username : "!", Template).
Template = "Welcome, ":username:"!".
% Template can later be evaluated with actual username
```

## Testing

Comprehensive tests have been added in `tests/test_call_to_variable.pl`:
- Tests for `starlog_call/2` with various expression types
- Tests for `starlog_eval/2` with evaluation scenarios
- Tests for `starlog_no_eval/2` with preservation scenarios
- Tests for combining these predicates with eval/no_eval wrappers
- Tests for error cases and edge conditions

## Documentation

Updated `README.md` with:
- New section "Saving Results to Variables"
- Documentation for all three new predicates
- Usage examples
- Use cases

## Demo

A demonstration file `demo_call_to_variable.pl` shows practical examples of using these predicates.

## Backward Compatibility

These additions are fully backward compatible:
- Existing code using `starlog_call/1` continues to work unchanged
- The new predicates are additive and don't modify existing behavior
- Module exports have been extended to include the new predicates

## Files Modified

1. `starlog.pl`: Added three new predicates and updated module exports
2. `README.md`: Added documentation for the new predicates
3. `tests/test_call_to_variable.pl`: Comprehensive test suite (new file)
4. `demo_call_to_variable.pl`: Demonstration examples (new file)
