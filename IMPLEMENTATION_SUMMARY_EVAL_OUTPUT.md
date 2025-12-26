# Implementation Summary: eval/no_eval Output Control

## Overview

This implementation adds options to control whether `eval()` and `no_eval()` wrappers are shown in the output when converting code to Starlog notation. 

## Problem Statement

The original requirement was:
> "Make the default not to output eval() and no_eval() but not necessarily the contents of (), and have options to output eval(), no_eval() or both."

## Solution

### Changes Made

1. **Modified `starlog_output_code/3`** - Added support for new options:
   - `output_eval(true/false)` - Controls whether `eval()` wrappers are shown
   - `output_no_eval(true/false)` - Controls whether `no_eval()` wrappers are shown
   - Default behavior: Both are set to `false` (strip both wrappers)

2. **Modified `starlog_output_file/3`** - Added the same options for file output

3. **Added new predicates**:
   - `strip_eval_no_eval_based_on_options/3` - Parses options and calls the stripping predicate
   - `strip_eval_no_eval/4` - Recursively strips `eval()` and/or `no_eval()` wrappers based on flags
   - `strip_eval_no_eval_arg/3` - Helper for processing arguments

4. **Updated documentation** in README.md with:
   - New section explaining the eval/no_eval output control
   - Usage examples showing all option combinations
   - Summary table of available options

5. **Added tests** in `tests/test_output_eval_options.pl`:
   - Test default behavior (strips both)
   - Test keeping eval only
   - Test keeping no_eval only
   - Test keeping both
   - Test nested eval/no_eval combinations
   - Test multiple expressions
   - Test complex nested structures
   - Test return value verification

6. **Added demo** in `demo_output_eval_options.pl`:
   - Demonstrates all option combinations
   - Shows comparison of different settings
   - Provides clear examples for users

## Default Behavior

By default (when no options are specified):
- `eval()` wrappers are **stripped** from the output
- `no_eval()` wrappers are **stripped** from the output
- The contents of these wrappers are preserved

This makes sense because:
1. Evaluation is the default behavior in Starlog, so `eval()` is redundant
2. Most users want cleaner output without unnecessary wrappers
3. The wrappers can be re-added if needed via options

## Options

| Option | Effect |
|--------|--------|
| `output_eval(false)` | Strip `eval()` wrappers (default) |
| `output_eval(true)` | Keep `eval()` wrappers in output |
| `output_no_eval(false)` | Strip `no_eval()` wrappers (default) |
| `output_no_eval(true)` | Keep `no_eval()` wrappers in output |

## Examples

```prolog
% Default - strips both
?- starlog_output_code(A is no_eval(1+1)).
A is 1+1

% Keep no_eval
?- starlog_output_code(A is no_eval(1+1), _, [output_no_eval(true)]).
A is no_eval(1+1)

% Keep eval
?- starlog_output_code(B is eval("x":"y"), _, [output_eval(true)]).
B is eval("x":"y")

% Keep both
?- starlog_output_code(C is no_eval(eval(1+1)), _, [output_eval(true), output_no_eval(true)]).
C is no_eval(eval(1+1))
```

## Implementation Details

### Recursive Stripping Algorithm

The stripping algorithm recursively processes the entire term structure:

1. **Handles all control structures**: conjunctions, disjunctions, if-then-else, negation
2. **Processes `is` expressions**: Strips from the right-hand side
3. **Strips wrappers conditionally**: Based on the StripEval and StripNoEval flags
4. **Recursively processes nested structures**: Handles compound terms and lists
5. **Preserves atomic terms**: Numbers, atoms, strings are returned as-is

### Integration Points

The stripping is applied at two points in the output process:

1. **After variable renaming**: When outputting Starlog code that's already in Starlog form
2. **After conversion and compression**: When converting from Prolog to Starlog form

This ensures that the stripping works correctly regardless of the input format.

## Compatibility

- All existing functionality is preserved
- Existing code will see no difference (default behavior strips wrappers)
- Options are backward-compatible (ignored if not provided)
- Can be combined with existing options like `compress(true)`

## Testing

Comprehensive tests cover:
- Default behavior verification
- Each option individually
- Combined options
- Nested structures
- Return value verification
- Edge cases

## Files Modified

1. `starlog_in_prolog.pl` - Core implementation
2. `README.md` - Documentation
3. `tests/test_output_eval_options.pl` - Tests (new)
4. `demo_output_eval_options.pl` - Demo (new)

## Future Enhancements

Potential improvements for the future:
- Add option to strip only at top level
- Add option to strip based on depth
- Add custom filtering predicates for more control
