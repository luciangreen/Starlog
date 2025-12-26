# Algebra Solver - Quick Reference

## Quick Start

```prolog
:- use_module(starlog_in_prolog).

% Solve equation
?- solve_equation((Y+5)/2 is 2, Y, Solution).
Solution = -1.
```

## Basic Syntax

```prolog
solve_equation(Equation, Variable, Solution)
solve_equation(Equation, Solution)  % Auto-detect variable
```

## Common Patterns

| Equation Type | Example | Solution |
|---------------|---------|----------|
| Addition | `Y+3 is 7` | `Y = 4` |
| Subtraction | `Y-2 is 5` | `Y = 7` |
| Multiplication | `Y*3 is 12` | `Y = 4` |
| Division | `Y/4 is 2` | `Y = 8` |
| Multi-step | `2*Y+3 is 11` | `Y = 4` |
| Nested | `(Y+1)*2 is 10` | `Y = 4` |
| Power | `Y**2 is 16` | `Y = 4` |

## How It Works

The solver applies inverse operations:
- **Division** → Multiply both sides
- **Multiplication** → Divide both sides
- **Addition** → Subtract from both sides
- **Subtraction** → Add to both sides
- **Power** → Take nth root of both sides

## Examples

### Problem Statement
```prolog
?- solve_equation((Y+5)/2 is 2, Y, Solution).
Solution = -1.
```

### Commutative Operations
```prolog
?- solve_equation(5+Y is 9, Y, Solution).
Solution = 4.

?- solve_equation(2*Y is 10, Y, Solution).
Solution = 5.
```

### Complex Nested
```prolog
?- solve_equation(((X+2)*3-1)/2 is 7, X, Solution).
Solution = 3.
```

## Testing

```bash
cd tests
swipl -s test_algebra_solver.pl      # Run algebra solver tests
swipl -s test_integrated_algebra.pl  # Run integration tests
```

## Demos

```bash
swipl -s demo_problem_statement.pl    # Problem statement demo
swipl -s demo_algebra_solver.pl       # Comprehensive demo
swipl -s interactive_algebra_demo.pl  # Interactive visual demo
```

## Files

- `algebra_solver.pl` - Core solver module
- `ALGEBRA_SOLVER_GUIDE.md` - Complete user guide
- `IMPLEMENTATION_SUMMARY_ALGEBRA.md` - Technical details

## Supported Operations

✓ Addition (`+`)  
✓ Subtraction (`-`)  
✓ Multiplication (`*`)  
✓ Division (`/`)  
✓ Power (`**`)  

## Limitations

- Single-variable equations only
- Variable must appear once in expression
- Algebraic operations only

## Learn More

See `ALGEBRA_SOLVER_GUIDE.md` for comprehensive documentation.
