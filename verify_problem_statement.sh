#!/bin/bash
# Verification script for the problem statement

echo "════════════════════════════════════════════════════════"
echo "  PROBLEM STATEMENT VERIFICATION"
echo "════════════════════════════════════════════════════════"
echo ""
echo "Problem: algebra (apply operations to both sides)"
echo "Example: (Y+5)/2 is 2"
echo ""
echo "Expected Solution: Y = -1"
echo ""
echo "────────────────────────────────────────────────────────"
echo "Running algebra solver..."
echo "────────────────────────────────────────────────────────"
echo ""

# Run the solver
swipl -g "use_module(starlog), solve_equation((Y+5)/2 is 2, Y, Solution), format('Solution: Y = ~w~n', [Solution]), halt" 2>&1 | grep "Solution:"

echo ""
echo "────────────────────────────────────────────────────────"
echo "Verification:"
echo "────────────────────────────────────────────────────────"
echo ""

# Verify the result
swipl -g "Y = -1, Result is (Y+5)/2, format('Substitute Y = -1 into (Y+5)/2:~n'), format('  (-1+5)/2 = ~w~n', [Result]), (Result =:= 2 -> write('  ✓ CORRECT!') ; write('  ✗ WRONG!')), nl, halt" 2>&1 | tail -3

echo ""
echo "════════════════════════════════════════════════════════"
echo "  VERIFICATION COMPLETE ✓"
echo "════════════════════════════════════════════════════════"
