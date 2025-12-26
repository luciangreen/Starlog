% ALGEBRA_SOLVER_GUIDE.md
# Algebra Solver - User Guide

## Overview

The Starlog algebra solver can solve algebraic equations by automatically applying inverse operations to both sides of an equation to isolate the variable.

## Problem Statement

From the problem statement: *"algebra (apply operations to both sides)"*
Example: `(Y+5)/2 is 2`

## How It Works

The solver applies inverse operations step by step:

1. **Division**: Multiplies both sides by the divisor
2. **Multiplication**: Divides both sides by the factor
3. **Addition**: Subtracts the addend from both sides
4. **Subtraction**: Adds the subtrahend to both sides
5. **Power**: Takes the nth root of both sides

## Usage

### Basic Usage

```prolog
:- use_module(algebra_solver).

% Solve equation: (Y+5)/2 = 2
?- solve_equation((Y+5)/2 is 2, Y, Solution).
Y = -1,
Solution = -1.
```

### Through Starlog Module

```prolog
:- use_module(starlog_in_prolog).

% The algebra solver is integrated
?- solve_equation((Y+5)/2 is 2, Y, Solution).
Y = -1,
Solution = -1.
```

### Auto-detect Variable

```prolog
% If you don't specify the variable, it will be auto-detected
?- solve_equation((Y+5)/2 is 2, Solution).
Solution = -1.
```

## Examples

### Simple Linear Equations

```prolog
% Addition: Y+3 = 7
?- solve_equation(Y+3 is 7, Y, Solution).
Solution = 4.

% Subtraction: Y-2 = 5
?- solve_equation(Y-2 is 5, Y, Solution).
Solution = 7.

% Multiplication: Y*3 = 12
?- solve_equation(Y*3 is 12, Y, Solution).
Solution = 4.

% Division: Y/4 = 2
?- solve_equation(Y/4 is 2, Y, Solution).
Solution = 8.
```

### Commutative Operations

```prolog
% The solver handles commutative operations automatically
?- solve_equation(5+Y is 9, Y, Solution).
Solution = 4.

?- solve_equation(2*Y is 10, Y, Solution).
Solution = 5.
```

### Multi-Step Equations

```prolog
% Multiple operations: 2*Y+3 = 11
?- solve_equation(2*Y+3 is 11, Y, Solution).
Solution = 4.

% Nested operations: (Y+1)*2 = 10
?- solve_equation((Y+1)*2 is 10, Y, Solution).
Solution = 4.

% Complex: ((X+2)*3-1)/2 = 7
?- solve_equation(((X+2)*3-1)/2 is 7, X, Solution).
Solution = 3.
```

### Variable on Right Side

```prolog
% The equation can have the variable on either side
?- solve_equation(2 is Y/3, Y, Solution).
Solution = 6.

?- solve_equation(10 is 2*Y, Y, Solution).
Solution = 5.
```

### Subtraction from Constant

```prolog
% Non-commutative operations are handled correctly
?- solve_equation(10-Y is 3, Y, Solution).
Solution = 7.

?- solve_equation(20-Y is 8, Y, Solution).
Solution = 12.
```

### Power Equations

```prolog
% Square: Y**2 = 16
?- solve_equation(Y**2 is 16, Y, Solution).
Solution = 4.

% Cube: Y**3 = 27
?- solve_equation(Y**3 is 27, Y, Solution).
Solution = 3.0.
```

## Step-by-Step Example

Let's solve `(Y+5)/2 = 2`:

```prolog
% Initial equation: (Y+5)/2 = 2
% Step 1: Multiply both sides by 2
%         Y+5 = 4
% Step 2: Subtract 5 from both sides
%         Y = -1

?- solve_equation((Y+5)/2 is 2, Y, Solution).
Solution = -1.

% Verify the solution
?- Y = -1, Result is (Y+5)/2.
Y = -1,
Result = 2.  % ✓ Correct!
```

## Supported Operations

- **Addition** (`+`)
- **Subtraction** (`-`)
- **Multiplication** (`*`)
- **Division** (`/`)
- **Power** (`**`)

## Limitations

1. The solver works with single-variable equations only
2. Complex expressions with the variable appearing multiple times may not be solvable
3. Non-algebraic operations are not supported
4. The solver finds one solution (for powers, it finds the principal root)

## Use Cases

- **Educational Tools**: Teaching algebra and equation solving
- **Mathematical Modeling**: Automated constraint solving
- **Problem Solving**: Programmatic solution of mathematical problems
- **Engineering Calculations**: Solving for unknown parameters

## Integration with Starlog

The algebra solver is fully integrated with the Starlog module and can be used alongside other Starlog features:

```prolog
:- use_module(starlog_in_prolog).

% Combine with Starlog operations
demo :-
    % Solve equation
    solve_equation(2*X is 10, X, Solution),
    
    % Use solution in Starlog expression
    Y is Solution + 5,
    
    % String concatenation
    Message is "The solution is: " : Y,
    
    write(Message), nl.
```

## Running Examples

```bash
# Run the problem statement demo
swipl -s demo_problem_statement.pl

# Run the comprehensive demo
swipl -s demo_algebra_solver.pl

# Run the test suite
cd tests
swipl -s test_algebra_solver.pl
```

## References

- Main module: `algebra_solver.pl`
- Tests: `tests/test_algebra_solver.pl`
- Demo: `demo_algebra_solver.pl`
- Problem statement demo: `demo_problem_statement.pl`
