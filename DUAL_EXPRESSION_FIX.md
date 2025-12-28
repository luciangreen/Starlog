# Dual Expression Fix: Preventing Infinite Backtracking

## Problem Statement

The issue addressed was:
```prolog
Please make [A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]].
A = [p] ; not hang afterwards.
```

## The Issue

When using dual Starlog expressions with nested list appends, the system could hang during backtracking, particularly when using predicates like `findall/3` that attempt to find all solutions.

### Example of the Problem

```prolog
% This would hang with findall before the fix
findall(A, starlog_call(([A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])), Solutions).
```

The hang occurred because:

1. The expression `[A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]]` compiles to:
   - LHS: A chain of append operations starting with unbound variable A
   - RHS: A chain of append operations starting with ground value [p]

2. Original goal order was:
   ```prolog
   append(A,[x],T1),        % A is unbound - generates infinite solutions!
   append(T1,[y],T2),
   append(T2,[z],Result),
   LHS = [Result],
   LHS = RHS,
   append([p],[x],R1),
   append(R1,[y],R2),
   append(R2,[z],RResult),
   RHS = [RResult]
   ```

3. The first `append(A,[x],T1)` with A unbound generates infinite solutions through backtracking:
   - A = [], T1 = [x]
   - A = [_], T1 = [_,x]
   - A = [_,_], T1 = [_,_,x]
   - ... (infinite)

## The Solution

The fix reorders the goals to execute the more ground side first and reverses the less ground side's pre-goals to ensure proper constraint propagation:

### Implementation

1. **Detect which side is more ground**: Count variables in each expression - the side with fewer variables is more ground.

2. **Split goals**: Separate each side's goals into pre-goals (the computation steps) and result goal (final unification).

3. **Reorder for constraint propagation**:
   ```prolog
   % New order:
   append([p],[x],R1),       % RHS pre-goals first (more ground)
   append(R1,[y],R2),
   append(R2,[z],RResult),
   RHS = [RResult],          % RHS result goal
   LHS = RHS,                % Unification
   LHS = [Result],           % LHS result goal - now Result is constrained!
   append(T2,[z],Result),    % LHS pre-goals in REVERSE - most constrained first
   append(T1,[y],T2),
   append(A,[x],T1)          % Now when we reach this, T1 is already constrained
   ```

4. **Why reversing works**: By executing `append(T2,[z],Result)` first where Result is already bound to [p,x,y,z], we constrain T2 = [p,x,y]. Then `append(T1,[y],T2)` with T2 bound constrains T1 = [p,x]. Finally `append(A,[x],T1)` with T1 bound constrains A = [p].

### Key Changes in starlog_expand.pl

1. Added `is_more_ground/2` predicate to compare groundness of expressions
2. Added `split_result_goal/3` to separate pre-goals from result goal
3. Modified dual expression handling to:
   - Check which side is more ground
   - Reverse LHS pre-goals when RHS is more ground
   - Order goals as: RHS pre-goals, RHS result, unification, LHS result, reversed LHS pre-goals

## Testing

The fix was verified with comprehensive tests:

```prolog
% Test 1: Exact problem statement
?- starlog_call(([A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])).
A = [p].  % ✓ Works correctly

% Test 2: No hang with findall
?- findall(A, starlog_call(([A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]])), Solutions).
Solutions = [[p]].  % ✓ Single solution, no infinite backtracking

% Test 3: Multiple nested appends
?- starlog_call(([A&[1]&[2]&[3]&[4]] is [[r]&[1]&[2]&[3]&[4]])).
A = [r].  % ✓ Works with longer chains
```

## Impact

- **No breaking changes**: All existing tests pass
- **Solves infinite backtracking**: findall and other backtracking operations now work correctly
- **Maintains correctness**: Results are identical, only execution order changed
- **Performance improvement**: Constrained execution is more efficient

## Files Modified

1. `starlog_expand.pl` - Core fix implementation
2. `tests/test_dual_expression_fix.pl` - Comprehensive test suite for the fix

## Related

This fix ensures that the problem statement requirement is fully met:
- `[A&[x]&[y]&[z]] is [[p]&[x]&[y]&[z]]` now works correctly with `A = [p]`
- The system does not hang afterwards, even with backtracking operations
