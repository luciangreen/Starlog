:- module(pr2_stage9_tests, [run_pr2_stage9_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage9_tests :-
    run_tests([pr2_stage9_codegen]).

:- begin_tests(pr2_stage9_codegen).

test(stage9_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage9_generate_code/2)),
    assertion(current_predicate(starlog:npl_stage9_emit_neurocode/2)),
    assertion(current_predicate(starlog:npl_stage9_compile_ir/2)).

test(stage9_closed_form_numeric_output_from_poly_node) :-
    starlog:npl_stage9_generate_code(lowered_ir([lowered_poly_eval(n, [0,0.5,0.5], result)]),
                                     generated_program(Statements)),
    member(assign(result, Expr), Statements),
    starlog:npl_substitute_index_atom(Expr, n, 6, GroundExpr),
    Value is GroundExpr,
    assertion(Value =:= 21).

test(stage9_direct_index_formula_loop_emission) :-
    Lowered = lowered_ir([direct_index_rule(index_spec([i]),
                                            [x-(4+i-1), y-(5+i-1)],
                                            result_collector(values))]),
    starlog:npl_stage9_generate_code(Lowered, generated_program(Statements)),
    assertion(member(loop([i],
                          [assign(x, 4+i-1), assign(y, 5+i-1)],
                          collect(values)),
                     Statements)).

test(stage9_loop_emission_handles_empty_relations) :-
    Lowered = lowered_ir([direct_index_rule(index_spec([i]), [], result_collector(values))]),
    starlog:npl_stage9_generate_code(Lowered, generated_program(Statements)),
    assertion(member(loop([i], [], collect(values)), Statements)).

test(stage9_loop_emission_supports_multiple_indices_and_collectors) :-
    Lowered = lowered_ir([direct_index_rule(index_spec([i,j]),
                                            [x-(i+j)],
                                            result_collector(records))]),
    starlog:npl_stage9_generate_code(Lowered, generated_program(Statements)),
    assertion(member(loop([i,j], [assign(x, i+j)], collect(records)), Statements)).

test(stage9_compiles_stage8_ir_to_neurocode) :-
    starlog:npl_stage8_build_ir(flow_graph(empty_flow_graph, map([]), [], trace([])),
                                [i],
                                [x-(4+i-1), y-(5+i-1)],
                                [0,1,1],
                                [],
                                IR),
    starlog:npl_stage9_compile_ir(IR, neurocode(Ops)),
    assertion(member(neuro_assign(poly_result, _), Ops)),
    assertion(member(neuro_loop([i],
                                [neuro_assign(x, 4+i-1), neuro_assign(y, 5+i-1)],
                                neuro_collect(values)),
                     Ops)).

test(stage9_mixed_symbolic_numeric_relations_are_preserved) :-
    starlog:npl_stage9_compile_ir(
        lowered_ir([
            direct_index_rule(index_spec([i]),
                              [sum-(i+1), cell-lookup(matrix,i), tag-symbol(i)],
                              result_collector(values))
        ]),
        neurocode(Ops)),
    assertion(member(neuro_loop([i],
                                [neuro_assign(sum, i+1),
                                 neuro_assign(cell, lookup(matrix,i)),
                                 neuro_assign(tag, symbol(i))],
                                neuro_collect(values)),
                     Ops)).

:- end_tests(pr2_stage9_codegen).
