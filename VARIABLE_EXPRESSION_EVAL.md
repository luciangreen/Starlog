# Variable Expression Evaluation Feature

## Problem Statement
"Complete A=([a]&[c]),B is A. B=[a,c] with all combinations and configurations, please."

## Solution Summary

This implementation enables automatic evaluation of Starlog expressions bound to variables when used with the `is` operator. The pattern:

```prolog
A = (StarlogExpr), B is A
```

is automatically transformed to:

```prolog
A = (StarlogExpr), starlog_eval(A, B)
```

This allows for more natural and intuitive code where Starlog expressions can be stored in variables and evaluated later.

## Examples

### Basic Usage

```prolog
% List append
A = ([a] & [c]),
B is A.
% B = [a,c]

% String concatenation
X = ("hello" : "world"),
Y is X.
% Y = "helloworld"

% Atom concatenation
P = (foo • bar),
Q is P.
% Q = foobar
```

### Complex Patterns

```prolog
% Nested expressions
L = (([1] & [2]) & [3]),
M is L.
% M = [1,2,3]

% Sequential operations
A = ([1] & [2]),
B is A,
C = (B & [3]),
D is C.
% B = [1,2], D = [1,2,3]

% Multiple bindings
X = ([a] & [b]),
Y is X,
Z = ("p" : "q"),
W is Z.
% Y = [a,b], W = "pq"
```

## Supported Operators

All three main Starlog operators are supported:

1. **List append (`&`)**: `A = ([1] & [2]), B is A` → `B = [1,2]`
2. **String concatenation (`:`)**: `A = ("x" : "y"), B is A` → `B = "xy"`
3. **Atom concatenation (`•`)**: `A = (a • b), B is A` → `B = ab`

## Files Modified

### starlog_expand.pl

Added a new pattern detection clause in `expand_goal_internal/2` (lines 73-95) that:
- Detects the pattern `Var1 = (StarlogExpr), (Var2 is Var1, ...)`
- Checks that the RHS contains Starlog operators but is not a complete Starlog goal
- Transforms the pattern to use `starlog_eval(Var1, Var2)`
- Handles both simple and compound conjunctions (with trailing goals)

## Files Created

### demo_variable_expression_evaluation.pl

A comprehensive demonstration file with 12 examples showing:
- Basic usage with all three operators
- Nested expressions
- Empty/identity elements
- Mixed operators
- Sequential operations
- Complex patterns

### tests/test_variable_expression_evaluation.pl

A complete test suite with 25 tests covering:
- Basic functionality for each operator
- Nested expressions
- Empty list handling
- Mixed operator usage
- Sequential operations
- Edge cases
- The exact problem statement from the issue

## How It Works

1. **Compile-time detection**: When a Prolog file is loaded, the term_expansion hook examines each clause body
2. **Pattern matching**: The expander looks for the pattern `Var1 = (StarlogExpr), (Var2 is Var1, ...)`
3. **Validation**: It checks that:
   - `Var1` is bound to an expression containing Starlog operators (`:`, `&`, `•`)
   - The expression is not already a complete Starlog goal (e.g., not `(X is Expr)`)
   - `Var2 is Var1` references the same variable
4. **Transformation**: The pattern is transformed to `Var1 = (StarlogExpr), starlog_eval(Var1, Var2)`
5. **Runtime evaluation**: When the code executes, `starlog_eval` evaluates the expression and binds the result to `Var2`

## Relationship to Existing Features

This feature complements the existing variable-bound goal pattern:
- **Existing**: `A = (C is Expr), A` → automatically executes the goal
- **New**: `A = (Expr), B is A` → automatically evaluates the expression

Both patterns make Starlog expressions more flexible as first-class values that can be stored and evaluated later.

## Limitations

- The pattern detection happens at compile-time (term expansion), so it works for clauses defined in loaded files
- Runtime goal construction (e.g., building goals dynamically) may still require explicit use of `starlog_call` or `starlog_eval`
- The pattern must appear in the clause body; it doesn't work inside meta-predicates or when goals are constructed dynamically

## Testing

All 25 tests pass successfully:

```bash
cd tests
swipl -g "run_tests, halt" -s test_variable_expression_evaluation.pl
```

Output:
```
% PL-Unit: variable_expression_evaluation ......................... passed
% All 25 tests passed
```

## Conclusion

This implementation completes the requirement to support `A = ([a] & [c]), B is A` with the result `B = [a,c]`, and extends it to work with all Starlog operator combinations and configurations. The feature integrates seamlessly with existing Starlog functionality and provides a more natural way to work with Starlog expressions as values.
