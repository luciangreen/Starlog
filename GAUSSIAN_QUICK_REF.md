# Gaussian Elimination Quick Reference

## Quick Start

```prolog
:- use_module(starlog_in_prolog).

% Solve a system: 2x + y = 5, x - y = 1
?- Solution is solve_system([[2, 1, 5], [1, -1, 1]]).
Solution = [2.0, 1.0].
```

## Matrix Format

```
[[a11, a12, ..., a1n, b1],
 [a21, a22, ..., a2n, b2],
 ...
 [am1, am2, ..., amn, bm]]
```

Where `aij` are coefficients and `bi` are constants.

## Common Patterns

### 2×2 System
```prolog
% ax + by = c
% dx + ey = f
Matrix = [[a, b, c], [d, e, f]]
```

### 3×3 System
```prolog
% ax + by + cz = d
% ex + fy + gz = h
% ix + jy + kz = l
Matrix = [[a, b, c, d], [e, f, g, h], [i, j, k, l]]
```

## Examples

### Example 1: Simple System
```prolog
% x + y = 3
% x - y = 1
?- solve_system([[1, 1, 3], [1, -1, 1]], [X, Y]).
X = 2.0,
Y = 1.0.
```

### Example 2: 3×3 System
```prolog
% 2x + y + 3z = 10
% 3x + 0y + 1.5z = 4.5
% 0x + 0y + z = 1
?- Solution is solve_system([[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]]).
Solution = [1.0, 5.0, 1].
```

### Example 3: Solution Type
```prolog
?- solve_system([[2, 1, 5], [1, -1, 1]], Solution, Type).
Solution = [2.0, 1.0],
Type = unique.
```

## Predicates

- `solve_system(+Matrix, -Solution)` - Solve and return solution
- `solve_system(+Matrix, -Solution, -Type)` - Solve with type detection
- `gaussian_elimination(+Matrix, -Reduced)` - Get reduced matrix
- `gaussian_elimination(+Matrix, -Reduced, -Type)` - Get reduced matrix with type

## Solution Types

- `unique` - Exactly one solution (full rank)
- `infinite` - Infinitely many solutions (underdetermined)
- `none` - No solution (inconsistent)

## See Also

- `GAUSSIAN_ELIMINATION.md` - Full documentation
- `demo_gaussian_elimination.pl` - Examples
- `demo_starlog_gaussian.pl` - Starlog syntax examples
- `tests/test_gaussian_elimination.pl` - Test suite
