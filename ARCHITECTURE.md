# Starlog Optimiser/Regeneration Architecture Notes

## Inspecting optimiser output

For the Stage 8/9 IR pipeline, the practical inspection flow is:

1. Build/lower IR (for example to `lowered_ir(...)`).
2. Generate inspectable Stage 9 statements with `npl_stage9_generate_code/2`.
3. Compile to neurocode with `npl_stage9_compile_ir/2` when machine-oriented output is needed.
4. Produce readable regenerated output with:
   - `npl_ir_to_annotated_source_text/3`
   - `npl_ir_to_annotated_source_file/3`

This keeps machine output (`neurocode(...)`) and human-readable inspection output separate.

## Compatibility naming (public guidance)

- `npl_stage9_generate_code/2`: low-level Stage 9 code generation.
- `npl_stage9_compile_ir/2`: direct Stage 9 compile entry point to neurocode.
- `npl_ir_to_annotated_source_text/3` and `npl_ir_to_annotated_source_file/3`: preferred APIs for readable inspection/regeneration artifacts.

## Example flow

```prolog
?- use_module(starlog).
?- IR = lowered_ir([lowered_poly_eval(n, [0,0.5,0.5], result)]),
   npl_stage9_generate_code(IR, Generated),
   npl_stage9_compile_ir(IR, Neurocode),
   npl_ir_to_annotated_source_text(IR, [source_file('examples/generated_input.pl')], Text).
```
