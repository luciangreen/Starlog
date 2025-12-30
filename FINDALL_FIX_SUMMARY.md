# Fix for findall with starlog_output_code and starlog_call

## Problem Statement
Fix all combinations and configurations of:
```prolog
starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result),C),
starlog_call(C).
```

## Root Cause Analysis

The issue occurred when trying to generate Starlog code from a `findall/3` call and then execute it:

1. **Incorrect compilation of meta-arguments**: The `findall/3` predicate was being treated as a regular value-returning builtin. This caused its template and goal arguments to be compiled as Starlog values, which is incorrect for meta-predicates.

2. **Variable renaming preventing execution**: The `starlog_output_code/2` function always renamed variables to human-friendly names (A, B, C, etc.) using `'$VAR'` terms. These renamed variables couldn't be unified when the generated code was passed to `starlog_call/1`.

3. **Infinite recursion risk**: Missing variable guard in `strip_eval_no_eval/4` could cause infinite loops when processing certain term structures.

## Solution Implemented

### 1. Special handling for findall (starlog_expand.pl)
Added a special case in `compile_starlog_expr/3` before the general value-returning builtin clause:

```prolog
compile_starlog_expr(findall(Template, Goal), Out, Goals) :-
    !,
    expand_goal_internal(Goal, ExpandedGoal),
    Goals = [findall(Template, ExpandedGoal, Out)].
```

This ensures:
- The template is kept as-is (can be any term including variables)
- The goal has its Starlog expressions expanded but is not treated as a value to compile
- The expansion produces correct `findall/3` calls

### 2. Optional variable renaming (starlog.pl)
Added a `rename(true/false)` option to `starlog_output_code/3`:

```prolog
% Default behavior:
% - rename(true) when print(true) - for display purposes
% - rename(false) when print(false) - for execution purposes
```

This allows generated code to be executed while maintaining readable output when printing.

### 3. Variable guard (starlog.pl)
Added a guard clause in `strip_eval_no_eval/4`:

```prolog
strip_eval_no_eval(Term, _StripEval, _StripNoEval, Term) :-
    var(Term),
    !.
```

This prevents infinite recursion when processing unbound variables.

## Testing

### Comprehensive Test Suite
Created `tests/test_findall_output_code.pl` with 12 tests covering:
- Basic pattern from problem statement
- All option combinations (print, rename, compress)
- Different goals (simple, compound, nested conditions)
- Different templates (simple, complex, constant)
- Edge cases (empty results, nested findall, multiple sequential calls)
- Integration with starlog_call

### Test Results
✅ All 12 new tests pass
✅ All existing tests pass (test_basic, test_output_code, test_find, test_nested_functions, etc.)
✅ No regressions introduced

## Examples

### Example 1: Basic Usage
```prolog
?- starlog_output_code(findall(X, (member(X, [1,2,3]), X > 1), Result), C),
   starlog_call(C).
Result = [2,3].
```

### Example 2: With Different Options
```prolog
?- starlog_output_code(
       findall(X, member(X, [a,b,c]), Result), 
       C, 
       [compress(true), print(false), rename(false)]
   ),
   starlog_call(C).
Result = [a,b,c].
```

### Example 3: Complex Template
```prolog
?- starlog_output_code(
       findall([A,B], (member(A, [1,2]), B is A*2), Result), 
       C
   ),
   starlog_call(C).
Result = [[1,2], [2,4]].
```

## Files Modified
- `starlog_expand.pl`: Added special handling for findall
- `starlog.pl`: Added rename option, variable guard, refactored for clarity
- `tests/test_findall_output_code.pl`: New comprehensive test suite

## Impact
This fix enables proper round-tripping of findall predicates through starlog_output_code and starlog_call, allowing developers to:
1. Convert Prolog findall calls to Starlog notation
2. Generate executable Starlog code programmatically
3. Use all option combinations (compress, print, rename) correctly
4. Work with complex templates and goals
