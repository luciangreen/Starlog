# find/2 Predicate - Quick Reference

## Specification

```prolog
% Starlog syntax (2 arguments):
Result is find(Template, Goal)

% Equivalent to:
findall(Template, (Goal, !), [Result])
% with automatic evaluation of Starlog expressions in Template

% Backward compatible 3-argument form:
find(Template, Goal, Result)  % Still available for compatibility
```

## Description

The `find/2` predicate executes a goal with a cut and collects the first solution. It follows the Starlog pattern of using `is` for value-returning operations, similar to `findall/2`.

## Signature

```prolog
% Starlog syntax (recommended):
Result is find(+Template, +Goal)

% Traditional Prolog syntax (backward compatible):
find(+Template, +Goal, -Result)
```

**Parameters:**
- `Template` - The variable pattern to collect
- `Goal` - The goal to execute (with automatic cut)
- `Result` - Unified with the first solution of Template

## Examples

### Basic Starlog Pattern
```prolog
?- starlog_call(Result is find(A, starlog_call([A:a] is [a:a]))).
Result = a.
```

### Member Query
```prolog
?- starlog_call(Result is find(X, member(X, [1,2,3]))).
Result = 1.  % Only first solution
```

### String Concatenation
```prolog
?- starlog_call(Result is find(R, starlog_call(R is "hello":"world"))).
Result = "helloworld".
```

### List Append
```prolog
?- starlog_call(Result is find(L, starlog_call(L is [1,2]&[3,4]))).
Result = [1, 2, 3, 4].
```

### Dual Expression
```prolog
?- starlog_call(Result is find(A, starlog_call(([1]&A) is (B&[2])))).
Result = [2].
```

### Arithmetic
```prolog
?- starlog_call(Result is find(X, X is 10+5)).
Result = 15.
```

## Use Cases

1. **Finding the first solution** to a goal when multiple solutions exist
2. **Executing Starlog expressions** and capturing the result
3. **Solving equations** with automatic cut behavior
4. **Pattern matching** with single solution extraction

## Behavior

- The **cut (!)** inside the findall ensures only the **first solution** is collected
- The result is extracted from a single-element list `[C]`
- If the goal fails, `find/2` fails
- If the goal has multiple solutions, only the first is returned
- Starlog expressions in the template are automatically evaluated

## Comparison with Similar Predicates

| Predicate | Syntax | Returns | Solutions |
|-----------|--------|---------|-----------|
| `findall/2` | `Result is findall(T, G)` | List of all solutions | All |
| `find/2` | `Result is find(T, G)` | Single result | First only (cut) |
| `once/1` | `once(G)` | Boolean success | First only (cut) |

## Migration from find/3

The old 3-argument form is still supported for backward compatibility:

```prolog
% Old syntax (still works):
find(A, starlog_call([A:a] is [a:a]), Result).

% New syntax (recommended):
starlog_call(Result is find(A, starlog_call([A:a] is [a:a]))).
```

## See Also

- `findall/2` - Starlog form of findall for collecting all solutions
- `findall/3` - Standard Prolog predicate for collecting all solutions
- `once/1` - Execute goal once with cut
- `starlog_call/1` - Execute Starlog goals
- `starlog_call/2` - Execute and capture result

## Testing

See `tests/test_find.pl` for comprehensive test suite with 10 test cases.

## Demo

Run `demo_find.pl` for interactive examples:
```bash
swipl -s demo_find.pl
```
