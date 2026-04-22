:- module(pr3_stage7_tests, [run_pr3_stage7_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr3_stage7_tests :-
    run_tests([pr3_stage7_error_handling]).

:- begin_tests(pr3_stage7_error_handling).

test(stage7_predicates_exist) :-
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_text/3)),
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_file/3)).

test(stage7_rejects_invalid_ir_shape, [throws(error(domain_error(npl_supported_ir_shape, invalid_ir_payload), _))]) :-
    starlog:npl_ir_to_annotated_source_text(invalid_ir_payload, [], _Text).

test(stage7_rejects_unsupported_ir_node, [throws(error(domain_error(npl_supported_ir_node, unsupported_node(foo)), _))]) :-
    starlog:npl_ir_to_annotated_source_text(lowered_ir([unsupported_node(foo)]), [], _Text).

test(stage7_rejects_invalid_output_file_type, [throws(error(type_error(text, 42), _))]) :-
    IR = lowered_ir([lowered_poly_eval(i, [0,1], poly_result)]),
    starlog:npl_ir_to_annotated_source_file(IR, [], 42).

test(stage7_keeps_supported_passthrough_generation) :-
    IR = lowered_ir([lowered_passthrough(ir_memo_site(cache_key, hit))]),
    starlog:npl_ir_to_annotated_source_text(IR, [], Text),
    assertion(sub_string(Text, _, _, _, 'npl_generated_passthrough(ir_memo_site(cache_key,hit)).')).

:- end_tests(pr3_stage7_error_handling).
