# find/3 Predicate - Quick Reference

## Specification

```prolog
find(A, B, C) :- findall(A, (B, !), [C]).
```

## Description

The `find/3` predicate executes a goal with a cut and collects the first solution. It's equivalent to using `findall/3` with a cut inside the goal to ensure only one solution is found.

## Signature

```prolog
find(+Template, +Goal, -Result)
```

**Parameters:**
- `Template` - The variable pattern to collect
- `Goal` - The goal to execute (with automatic cut)
- `Result` - Unified with the first solution of Template

## Examples

### Basic Starlog Pattern
```prolog
?- find(A, starlog_call([A:a] is [a:a]), Result).
Result = a.
```

### Member Query
```prolog
?- find(X, member(X, [1,2,3]), Result).
Result = 1.  % Only first solution
```

### String Concatenation
```prolog
?- find(R, starlog_call(R is "hello":"world"), Result).
Result = "helloworld".
```

### List Append
```prolog
?- find(L, starlog_call(L is [1,2]&[3,4]), Result).
Result = [1, 2, 3, 4].
```

### Dual Expression
```prolog
?- find(A, starlog_call(([1]&A) is (B&[2])), Result).
Result = [2].
```

### Arithmetic
```prolog
?- find(X, X is 10+5, Result).
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
- If the goal fails, `find/3` fails
- If the goal has multiple solutions, only the first is returned

## Comparison with Similar Predicates

| Predicate | Returns | Solutions |
|-----------|---------|-----------|
| `findall/3` | List of all solutions | All |
| `find/3` | Single result | First only (cut) |
| `once/1` | Boolean success | First only (cut) |

## See Also

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
