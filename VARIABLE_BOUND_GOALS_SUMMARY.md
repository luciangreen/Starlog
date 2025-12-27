# Variable-Bound Starlog Goals - Implementation Summary

## Problem Statement

The user wanted the following Prolog query to work:
```prolog
?- A=(C is no_eval(eval(1+1))),A.
```

Previously, this would fail with:
```
ERROR: is/2: Arithmetic: `no_eval/1' is not a function
```

## Root Cause

The issue occurred because:
1. When `A=(C is no_eval(eval(1+1)))` is executed, `A` is unified with the term `(C is no_eval(eval(1+1)))`
2. This term is stored as data, not as an executable goal
3. When `A` is later executed, it tries to execute the raw term without going through Starlog's goal expansion
4. Since `no_eval/1` is not a valid Prolog arithmetic function, the execution fails

## Solution

The solution implements automatic pattern detection during goal expansion:

1. **Pattern Detection**: During goal expansion, detect the pattern `Var=(StarlogGoal),Var`
2. **Automatic Transformation**: Transform it to `Var=(StarlogGoal),starlog_call(Var)`
3. **Proper Expansion**: `starlog_call/1` properly expands the Starlog expression before execution

### Technical Implementation

#### Changes to `starlog_expand.pl`:
- Added pattern matching in `expand_goal_internal/2` to detect `Var=(StarlogGoal),Var`
- Added helper predicates:
  - `contains_starlog_in_term/1`: Check if a term contains Starlog expressions
  - `is_var_or_starts_with_var/2`: Check if a goal starts with a specific variable
  - `replace_first_var_with_starlog_call/3`: Replace variable execution with `starlog_call/1`

#### Changes to `starlog.pl`:
- Added `prolog:call_hook/2` for runtime goal expansion support
- Added `might_be_starlog_goal/1` for performance optimization

## Testing

### New Tests
Created `tests/test_variable_bound_goals.pl` with comprehensive tests:
- Variable-bound goal with no_eval and eval
- Simple Starlog expressions
- List operations
- Multiple variable-bound goals
- Nested expressions
- Complex expressions
- Arithmetic expressions

### Existing Tests
All existing tests pass:
- `test_eval.pl` - ✓ All 12 tests pass
- `test_no_eval.pl` - ✓ All 8 tests pass
- `test_basic.pl` - ✓ All 6 tests pass
- `test_nested.pl` - ✓ All 5 tests pass
- `test_arithmetic_is.pl` - ✓ All 5 tests pass
- `test_mixed_prolog_starlog.pl` - ✓ All 5 tests pass

## Usage

### At Interactive REPL
```prolog
?- use_module(starlog).
?- A=(C is no_eval(eval(1+1))),A.
C = 2.
```

### In Prolog Files
```prolog
:- use_module(starlog).

test :-
    A=(C is no_eval(eval(1+1))),A,
    write('C = '), write(C), nl.
```

### For Non-Immediate Execution
```prolog
process_goal(Goal) :-
    starlog_call(Goal).

?- MyGoal = (X is "a":"b"), process_goal(MyGoal).
X = "ab".
```

## Limitations

1. **Command-line `-g` goals**: The pattern detection does NOT work with `-g` command-line goals because they bypass goal expansion. Use `starlog_call/1` instead:
   ```bash
   swipl -g "use_module(starlog), starlog_call(C is no_eval(eval(1+1))), write(C), nl." -t halt
   ```

2. **Non-immediate execution**: If the variable is not executed immediately after binding, use `starlog_call/1` explicitly.

## Performance Considerations

- Pattern detection happens at compile-time (goal expansion), so there's no runtime overhead for the transformation
- Added `might_be_starlog_goal/1` quick check to filter non-Starlog goals before attempting expansion in the call hook
- The call hook only triggers when goals are explicitly called via `call/1`

## Documentation

Updated `README.md` with:
- New section "Variable-Bound Starlog Goals" explaining the feature
- Example 6 showing usage patterns
- Notes about limitations and when to use `starlog_call/1`

Created `demo_variable_bound_goals.pl` demonstrating:
- Basic variable-bound goals
- String concatenation
- List operations
- Multiple goals
- Using starlog_call for non-immediate execution
- Complex nested expressions

## Security Analysis

No security vulnerabilities identified. CodeQL analysis shows no issues with the changes.
