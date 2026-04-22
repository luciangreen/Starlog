:- module(pr2_stage8_tests, [run_pr2_stage8_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage8_tests :-
    run_tests([pr2_stage8_ir_pipeline]).

:- begin_tests(pr2_stage8_ir_pipeline).

test(stage8_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage8_pipeline_order/1)),
    assertion(current_predicate(starlog:npl_stage8_build_ir/6)),
    assertion(current_predicate(starlog:npl_stage8_ir_provenance/2)),
    assertion(current_predicate(starlog:npl_stage8_lower_ir/2)).

test(stage8_pipeline_order_is_documented_and_deterministic) :-
    starlog:npl_stage8_pipeline_order(PassOrderA),
    starlog:npl_stage8_pipeline_order(PassOrderB),
    assertion(PassOrderA == PassOrderB),
    assertion(PassOrderA == [
        parse_and_analyse,
        semantic_analysis,
        recurrence_classification,
        sample_extraction,
        degree_estimation,
        gaussian_elimination_polynomial_solve,
        polynomial_validation_and_rewrite,
        symbolic_index_assignment,
        predicate_reduction_pattern_irreducibles,
        indexed_variable_flow_tracing,
        independent_variable_identification,
        formula_reconstruction_from_first_principles,
        gaussian_elimination_indexed_polynomial_formula_solve,
        direct_index_rule_reconstruction,
        simplification,
        code_generation
    ]).

test(stage8_ir_contains_required_nodes_and_metadata) :-
    Relations = [u-(i^2+i), x-(4+i-1), y-(5+i-1)],
    Coefficients = [0,1,1],
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [i],
                                Relations,
                                Coefficients,
                                [provenance(derived_from_tests), rational_coefficients(true)],
                                IR),
    IR = ir_pipeline(_Passes, Nodes, meta(Metadata)),
    assertion(length(Nodes, 3)),
    assertion(Nodes == [
        ir_provenance(derived_from_tests, ir_index_relation([i], [u-(i^2+i),x-(4+i-1),y-(5+i-1)])),
        ir_provenance(derived_from_tests, ir_poly_eval(i, [rational(0),rational(1),rational(1)], poly_result)),
        ir_provenance(derived_from_tests, ir_direct_index_rule(index_spec([i]), relations([u-(i^2+i),x-(4+i-1),y-(5+i-1)]), result_collector(values)))
    ]),
    assertion(member(coefficient_representation(rational), Metadata)).

test(stage8_ir_build_is_deterministic_for_unordered_inputs) :-
    RelationsA = [y-(5+i-1), x-(4+i-1), u-(i^2+i)],
    RelationsB = [u-(i^2+i), x-(4+i-1), y-(5+i-1)],
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])), [i], RelationsA, [0,1,1], [], IRA),
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])), [i], RelationsB, [0,1,1], [], IRB),
    assertion(IRA == IRB).

test(stage8_ir_lowering_preserves_direct_rule_semantics) :-
    Relations = [x-(4+i-1), y-(5+i-1)],
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])), [i], Relations, [1,1], [], IR),
    starlog:npl_stage8_lower_ir(IR, lowered_ir(LoweredNodes)),
    assertion(member(lowered_index_relation([i], _), LoweredNodes)),
    assertion(member(direct_index_rule(index_spec([i]), _, result_collector(values)), LoweredNodes)).

test(stage8_ir_provenance_is_inspectable) :-
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [i],
                                [x-(4+i-1)],
                                [3,1],
                                [provenance(stage8_trace_lineage)],
                                IR),
    starlog:npl_stage8_ir_provenance(IR, Provenance),
    assertion(Provenance == [stage8_trace_lineage]).

test(stage8_ir_handles_empty_coefficients_without_poly_node) :-
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [i],
                                [x-(4+i-1)],
                                [],
                                [],
                                ir_pipeline(_, Nodes, _)),
    assertion(\+ member(ir_provenance(_, ir_poly_eval(_, _, _)), Nodes)).

test(stage8_ir_keeps_zero_and_negative_coefficients_in_rational_mode) :-
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [k],
                                [u-(k^2-k)],
                                [0,-1,1],
                                [rational_coefficients(true)],
                                ir_pipeline(_, Nodes, _)),
    assertion(member(ir_provenance(stage8_first_principles_derivation,
                                   ir_poly_eval(k, [rational(0),rational(-1),rational(1)], poly_result)),
                    Nodes)).

:- end_tests(pr2_stage8_ir_pipeline).
