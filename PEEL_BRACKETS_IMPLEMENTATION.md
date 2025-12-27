# Peel Off Nested Brackets Implementation

## Summary

This document describes the implementation of the `peel_off_brackets/2` feature that converts a list containing a string into a list with nested Starlog expressions.

## Problem Statement

"Peel off nested brackets from ["543"] is [5:(2+2):A]."

## Solution

The implementation provides a new predicate `peel_off_brackets/2` that transforms a list containing a string (e.g., `["543"]`) into a list with a nested Starlog expression (e.g., `[5:(2+2):_]`).

### Transformation Rules

- Character '5' → number 5
- Character '4' → expression (2+2)
- Character '3' → anonymous variable
- All elements joined with `:` operator (string concatenation)

## Implementation Details

### Files Created

1. **peel_brackets.pl** - Main module with the transformation logic
   - `peel_off_brackets/2` - Main predicate
   - `chars_to_expression/2` - Converts character list to expression
   - `char_to_element/2` - Converts individual characters

2. **tests/test_peel_brackets.pl** - Test suite
   - Tests basic transformation
   - Tests with atom input
   - Tests single and two character strings

3. **demo_peel_brackets.pl** - Demonstration file
   - Shows usage examples
   - Explains the transformation

### Integration

- Added to `starlog.pl` module exports
- Included in module dependencies
- Documented in README.md

## Usage Examples

```prolog
% Basic usage
?- peel_off_brackets(["543"], Result).
Result = [5:(2+2):_].

% Single character
?- peel_off_brackets(["5"], Result).
Result = [5].

% Two characters
?- peel_off_brackets(["54"], Result).
Result = [5:(2+2)].

% With atom input
?- peel_off_brackets(['543'], Result).
Result = [5:(2+2):_].
```

## Testing

All tests pass:
- ✓ Basic peel off with string input
- ✓ Peel off with atom input
- ✓ Single character handling
- ✓ Two character handling
- ✓ Existing starlog tests remain passing

## Security

No security vulnerabilities detected. CodeQL analysis shows no issues.

## Educational Use Cases

This feature is useful for:
- Demonstrating Starlog syntax transformations
- Teaching pattern matching and data conversion
- Converting numeric strings into expressions
- Educational demonstrations of functional notation
