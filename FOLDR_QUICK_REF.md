# foldr Quick Reference

## Syntax

### Prolog Style
```prolog
foldr(Operation, List, Accumulator, Result)
```

### Starlog Style
```prolog
Result is foldr(Operation, List, Accumulator)
```

## Examples

### String Reversal (Problem Statement)
```prolog
% Prolog
?- string_chars("abc", C), reverse(C, R), foldr(string_concat, R, "", Result).
Result = "cba".

% Starlog
?- starlog_call(R is foldr(string_concat, reverse(string_chars("abc")), "")).
R = "cba".
```

### List Concatenation
```prolog
% Prolog
?- foldr(append, [[1], [2], [3]], [], Result).
Result = [1, 2, 3].

% Starlog
?- starlog_call(R is foldr(append, [[1], [2], [3]], [])).
R = [1, 2, 3].
```

### String Building from Characters
```prolog
?- foldr(string_concat, [h, e, l, l, o], "", Result).
Result = "hello".
```

## How It Works

`foldr(f, [x1, x2, x3], acc)` computes: `f(x1, f(x2, f(x3, acc)))`

Processing order: **right to left** (processes `x3` first, then `x2`, then `x1`)

## See Also
- Full documentation: FOLDR_IMPLEMENTATION.md
- Tests: tests/test_foldr.pl
- Demo: demo_foldr.pl
- Verification: verify_foldr.sh
