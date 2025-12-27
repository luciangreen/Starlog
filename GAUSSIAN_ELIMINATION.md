# Gaussian Elimination Solver

## Overview

The Gaussian elimination solver is a module that solves systems of linear equations using the Gaussian elimination algorithm with partial pivoting. It supports both systems with unique solutions and systems with infinite solutions (underdetermined systems).

## Usage

### Basic Usage

```prolog
:- use_module(gaussian_elimination).

% Solve a 3x3 system
% 2x + y + 3z = 10
% 3x + 0y + 1.5z = 4.5
% 0x + 0y + z = 1

?- Matrix = [[2, 1, 3, 10], [3, 0, 1.5, 4.5], [0, 0, 1, 1]],
   solve_system(Matrix, Solution).
Solution = [1.0, 5.0, 1].
```

### Starlog Syntax

The solver can be used with Starlog syntax:

```prolog
:- use_module(starlog).

?- Solution is solve_system([[2, 1, 5], [1, -1, 1]]).
Solution = [2.0, 1.0].
```

## Predicates

### solve_system/2

```prolog
solve_system(+AugmentedMatrix, -Solution)
```

Solves a system of linear equations represented as an augmented matrix.

**Parameters:**
- `AugmentedMatrix`: List of rows, where each row is `[coeff1, coeff2, ..., constant]`
- `Solution`: List of values for variables `[x1, x2, ...]`

**Example:**
```prolog
?- solve_system([[2, 1, 5], [1, -1, 1]], Solution).
Solution = [2.0, 1.0].
```

### solve_system/3

```prolog
solve_system(+AugmentedMatrix, -Solution, -SolutionType)
```

Solves a system and returns the solution type.

**Parameters:**
- `AugmentedMatrix`: List of rows, where each row is `[coeff1, coeff2, ..., constant]`
- `Solution`: List of values for variables `[x1, x2, ...]`
- `SolutionType`: One of `unique`, `infinite`, or `none`

**Example:**
```prolog
?- solve_system([[2, 1, 5], [1, -1, 1]], Solution, Type).
Solution = [2.0, 1.0],
Type = unique.
```

### gaussian_elimination/2

```prolog
gaussian_elimination(+Matrix, -ReducedMatrix)
```

Performs Gaussian elimination on an augmented matrix to obtain row echelon form.

**Parameters:**
- `Matrix`: Original augmented matrix
- `ReducedMatrix`: Matrix in row echelon form

### gaussian_elimination/3

```prolog
gaussian_elimination(+Matrix, -ReducedMatrix, -SolutionType)
```

Performs Gaussian elimination and detects the solution type.

**Parameters:**
- `Matrix`: Original augmented matrix
- `ReducedMatrix`: Matrix in row echelon form
- `SolutionType`: One of `unique`, `infinite`, or `none`

## Algorithm

The solver uses the Gaussian elimination algorithm with the following steps:

1. **Forward Elimination**: Transform the matrix to row echelon form
   - Find pivot element (largest absolute value in column)
   - Swap rows if needed to bring pivot to diagonal
   - Eliminate entries below pivot

2. **Solution Type Detection**: Determine if system has unique, infinite, or no solution
   - Check for inconsistency (0 = nonzero)
   - Count variables with pivots (leading coefficients)
   - If all variables have pivots: unique solution
   - If some variables lack pivots: infinite solutions (free parameters)

3. **Back Substitution**: Solve for variables starting from last row
   - For unique solutions: compute exact values
   - For infinite solutions: variables without pivots remain free (unbound)

## Matrix Format

Matrices are represented as lists of lists in augmented form:
```
[[a11, a12, ..., a1n, b1],
 [a21, a22, ..., a2n, b2],
 ...
 [am1, am2, ..., amn, bm]]
```

Where:
- `aij` are the coefficients
- `bi` are the constants on the right-hand side
- The system represents: `a11*x1 + a12*x2 + ... + a1n*xn = b1`, etc.

## Examples

### Example 1: Unique Solution

```prolog
% System:
%   2x + y = 5
%   x - y = 1
% Solution: (2, 1)

?- solve_system([[2, 1, 5], [1, -1, 1]], Solution).
Solution = [2.0, 1.0].
```

### Example 2: 3x3 System

```prolog
% System:
%   x + 2y + 3z = 14
%   2x + y + z = 9
%   3x + 2y + z = 13
% Solution: (2.25, 1.75, 2.75)

?- solve_system([[1, 2, 3, 14], [2, 1, 1, 9], [3, 2, 1, 13]], Solution).
Solution = [2.25, 1.75, 2.75].
```

### Example 3: Underdetermined System (Infinite Solutions)

```prolog
% System (2 equations, 3 unknowns):
%   x + y + z = 6
%   2x + y + 3z = 14
% Has infinite solutions (one free variable)

?- solve_system([[1, 1, 1, 6], [2, 1, 3, 14]], Solution, Type).
Type = infinite.
```

### Example 4: Parametric System with Free Variable

```prolog
% System with free variable x₂:
%   2x₁ + 0x₂ + 3x₃ = 2
%   3x₁ + 0x₂ + 0x₃ = 1
%   0x₁ + 0x₂ + 2.25x₃ = 1
% Solution: (x₁, x₂, x₃) = (1/3, t, 4/9) where t is any value

?- solve_system([[2, 0, 3, 2], [3, 0, 0, 1], [0, 0, 2.25, 1]], Solution, Type).
Type = infinite,
Solution = [0.333333..., _FreeVar, 0.444444...].

% The solution shows:
%   x₁ = 1/3 ≈ 0.333...
%   x₂ = t (free parameter, shown as unbound variable)
%   x₃ = 4/9 ≈ 0.444...
```

## Features

- ✅ Solves systems with unique solutions
- ✅ Detects systems with infinite solutions (underdetermined)
- ✅ Detects inconsistent systems (no solution)
- ✅ Partial pivoting for numerical stability
- ✅ Handles floating-point arithmetic
- ✅ Integrated with Starlog syntax

## Implementation Notes

### Numerical Precision

The solver uses a tolerance of `1e-10` for checking if values are effectively zero. This helps handle floating-point arithmetic errors.

### Pivot Selection

The algorithm uses partial pivoting, selecting the row with the largest absolute value in each column as the pivot. This improves numerical stability.

### Solution Types

- **Unique**: System has exactly one solution (all variables have pivots)
- **Infinite**: System is underdetermined (some variables lack pivots, creating free parameters)
- **None**: System is inconsistent (row with all zeros except constant)

### Free Variables

For systems with infinite solutions, the solution may contain unbound Prolog variables representing free parameters. These can take any value.

## Related Modules

- `starlog`: Main Starlog library that exports these predicates
- `algebra_solver`: Algebraic equation solver for single-variable equations

## Testing

Run the test suite:

```bash
swipl -g "run_all_tests" -t halt tests/test_gaussian_elimination.pl
```

## Demo Programs

- `demo_gaussian_elimination.pl`: Basic examples and usage
- `demo_starlog_gaussian.pl`: Using Gaussian elimination with Starlog syntax
- `demo_parametric_system.pl`: Parametric system with free variables (x₁=1/3, x₂=t, x₃=4/9)

## References

- Gaussian elimination: https://en.wikipedia.org/wiki/Gaussian_elimination
- Linear systems: https://en.wikipedia.org/wiki/System_of_linear_equations
