# Running the tests

## Requirements

- SWI-Prolog 8.x or newer (`swipl`)
- Node.js for the Child Prolog browser-engine test

Run commands from the repository root.

## Complete test suite

```sh
sh tests/run_all_tests.sh
```

The script runs every standalone `tests/test_*.pl` file, the staged PR2/PR3
suite in `tests/run_tests.pl`, and `tests/test_child_prolog_engine.js`. It
stops on the first command that returns a failure status.

## Focused test runs

Run one Prolog test file at a time:

```sh
swipl -q -s tests/test_basic.pl
swipl -q -s tests/test_nested_functions.pl
swipl -q -s tests/test_method_chain.pl
```

Run the staged optimiser and annotated-regeneration tests:

```sh
swipl -q -s tests/run_tests.pl
```

Run the JavaScript engine test:

```sh
node tests/test_child_prolog_engine.js
```

## Feature coverage

| Feature area | Representative tests |
|---|---|
| Starlog operators, value builtins, and arithmetic preservation | `test_basic.pl`, `test_arithmetic_is.pl`, `test_arithmetic_in_builtins.pl` |
| Nested calls, method chains, and variable-bound goals | `test_nested_functions.pl`, `test_method_chain.pl`, `test_variable_bound_goals.pl` |
| `eval/1`, `no_eval/1`, and expression evaluation | `test_eval.pl`, `test_no_eval.pl`, `test_variable_expression_evaluation.pl` |
| Dual expressions and list/string/atom constraints | `test_dual_expr_is.pl`, `test_list_dual_expressions.pl`, `test_list_append_concat_constraints.pl` |
| Builtin helpers, `find/2`, `foldr/4`, and `=..` conversions | `test_find.pl`, `test_find_template_evaluation.pl`, `test_foldr.pl`, `test_univ_conversion.pl` |
| Prolog/Starlog code output, conversion, formatting, and compression | `test_output_code.pl`, `test_output_style.pl`, `test_starlog_to_prolog.pl`, `test_compression.pl` |
| Algebra solver and Gaussian elimination | `test_algebra_solver.pl`, `test_integrated_algebra.pl`, `test_gaussian_elimination.pl` |
| PR2 indexed tracing, IR, and PR3 regeneration/code generation | `pr2_stage*_tests.pl`, `pr3_stage*_tests.pl` via `run_tests.pl` |
| Child Prolog engine used by the browser demo | `test_child_prolog_engine.js` |

The representative tests are examples, not an exhaustive listing; the complete
script runs all standalone Prolog tests and all staged tests.
