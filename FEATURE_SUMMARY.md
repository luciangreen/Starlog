# Starlog Code Output Feature - Implementation Summary

## Overview

This implementation adds a feature to output Prolog code as Starlog notation with human-friendly variable names (A, B, C, ..., Z, A1, B1, etc.) instead of internal Prolog variables like _58, _1020, etc.

## Feature Requested

From the issue: "Include a feature to output the file(s)' or call's Starlog code. This may involve using old code to have extra variables A1, B1, etc."

## Implementation Details

### 1. Variable Renaming System

#### `generate_var_name/2`
Generates human-friendly variable names from an index:
- 0-25 → A-Z
- 26-51 → A1-Z1
- 52-77 → A2-Z2
- And so on...

Example:
```prolog
?- generate_var_name(0, Name).
Name = 'A'.

?- generate_var_name(25, Name).
Name = 'Z'.

?- generate_var_name(26, Name).
Name = 'A1'.
```

#### `rename_variables/2`
Renames all variables in a term to human-friendly names using SWI-Prolog's standard `$VAR/1` mechanism.

### 2. Code Output Predicates

#### `starlog_output_code/1`
Converts and prints a Prolog goal in Starlog notation:
```prolog
?- starlog_output_code(string_concat("x", "y", C)).
A is "x":"y"
```

#### `starlog_output_code/2`
Same as above but also returns the Starlog code as a term.

#### `starlog_output_file/1`
Converts an entire Prolog file to Starlog notation:
```prolog
?- starlog_output_file('my_program.pl').
% Starlog code output for file: my_program.pl

greet(A,B,C):-D is "Hello, ":A,E is D:" ",C is E:B.
...
```

#### `starlog_output_file/2`
Same as above but writes to a specific output stream.

### 3. Prolog to Starlog Conversion

The `convert_prolog_to_starlog/2` predicate handles:

- **String concatenation**: `string_concat(A, B, C)` → `C is (A:B)`
- **List append**: `append(A, B, C)` → `C is (A&B)`
- **Atom concatenation**: `atom_concat(A, B, C)` → `C is (A•B)`
- **Value-returning builtins**: `reverse([1,2,3], R)` → `R is reverse([1,2,3])`
- **Nullary builtins**: `get_time(T)` → `T is get_time`
- **Already Starlog code**: Preserves existing Starlog notation

## Examples

### Example 1: Simple Conversion
```prolog
?- starlog_output_code(append([1,2], [3,4], L)).
A is [1,2]&[3,4]
```

### Example 2: Multiple Variables
```prolog
?- starlog_output_code((string_concat(X, Y, Z), append(Z, W, R))).
A is B:C,D is A&E
```

### Example 3: Complex Nested
```prolog
?- starlog_output_code((
    string_concat("hello", " ", T1),
    string_concat(T1, "world", T2),
    append([1,2], [3,4], L),
    reverse(L, R)
)).
A is "hello":" ",B is A:"world",C is [1,2]&[3,4],D is reverse(C)
```

### Example 4: File Conversion
Given `sample.pl`:
```prolog
greet(First, Last, Greeting) :-
    string_concat("Hello, ", First, Temp),
    string_concat(Temp, " ", Temp2),
    string_concat(Temp2, Last, Greeting).
```

Output:
```prolog
?- starlog_output_file('sample.pl').
% Starlog code output for file: sample.pl

greet(A,B,C):-D is "Hello, ":A,E is D:" ",C is E:B.
```

## Testing

### Test Coverage

1. **test_output_code.pl**: Tests basic code conversion
   - Simple string concatenation
   - List append
   - Atom concatenation
   - Value-returning builtins
   - Already Starlog code
   - Multiple variables

2. **test_output_file.pl**: Tests file conversion

3. **test_output_to_file.pl**: Tests writing output to a file

4. **test_generated_starlog.pl**: Validates that generated code is executable

5. **demo_output_feature.pl**: Comprehensive demonstration

### Test Results

All tests pass:
- ✅ 6/6 basic tests
- ✅ 5/5 nested expression tests
- ✅ 5/5 arithmetic preservation tests
- ✅ 5/5 mixed Prolog/Starlog tests
- ✅ 7/7 compound with operators tests
- ✅ All new output tests pass
- ✅ Demo runs successfully

## Use Cases

1. **Code Documentation**: Generate readable Starlog code for documentation
2. **Code Migration**: Convert existing Prolog code to Starlog notation
3. **Teaching**: Show students how Prolog predicates map to Starlog
4. **Debugging**: Display code in a more readable format
5. **Code Generation**: Programmatically generate Starlog code

## API Documentation

### Exported Predicates

```prolog
starlog_output_code(+Goal)
starlog_output_code(+Goal, -StarlogCode)
starlog_output_file(+FilePath)
starlog_output_file(+FilePath, +OutputStream)
```

### Internal Predicates

```prolog
generate_var_name(+Index, -Name)
rename_variables(+Term, -RenamedTerm)
rename_vars_list(+Vars, +StartIndex)
convert_prolog_to_starlog(+PrologGoal, -StarlogForm)
is_already_starlog(+Goal)
contains_starlog_op(+Expr)
```

## Files Modified/Added

### Modified
- `starlog_in_prolog.pl`: Added new predicates (175 lines added)
- `README.md`: Added "Outputting Starlog Code" section

### Added
- `.gitignore`: Exclude generated files
- `tests/test_output_code.pl`: Test code conversion
- `tests/test_output_file.pl`: Test file conversion
- `tests/test_output_to_file.pl`: Test writing to file
- `tests/test_generated_starlog.pl`: Test generated code execution
- `tests/sample_prolog.pl`: Sample file for testing
- `demo_output_feature.pl`: Comprehensive demonstration

## Technical Notes

1. **Variable Naming**: Uses SWI-Prolog's standard `$VAR/1` mechanism, which is compatible with `numbervars/3` and `write_term/3`

2. **Conversion Strategy**: The conversion is "best effort" - if a pattern isn't recognized, it's kept as-is

3. **Preservation**: Already-Starlog code is preserved without modification

4. **Extensibility**: Works with all registered value-returning builtins in `starlog_registry.pl`

## Future Enhancements

Potential improvements for future versions:
1. Support for more complex Prolog patterns (if-then-else, cuts, etc.)
2. Pretty-printing with indentation
3. Configurable variable naming schemes
4. Batch file conversion utilities
5. Integration with version control for code migration

## Conclusion

This feature successfully implements Prolog-to-Starlog code output with human-friendly variable names as requested. All tests pass and the implementation is well-documented and extensible.
