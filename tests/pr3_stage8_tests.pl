:- module(pr3_stage8_tests, [run_pr3_stage8_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr3_stage8_tests :-
    run_tests([pr3_stage8_regeneration_stability]).

:- begin_tests(pr3_stage8_regeneration_stability).

test(stage8_public_regeneration_predicates_exist) :-
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_text/3)),
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_file/3)).

test(stage8_stage9_generation_surface_is_reachable) :-
    assertion(current_predicate(starlog:npl_stage9_generate_code/2)).

test(stage8_annotated_regeneration_is_stable_for_same_ir) :-
    IR = lowered_ir([
        lowered_poly_eval(x, [0,1], y),
        direct_index_rule(index_spec([x]), [z-(x+1)], result_collector(values))
    ]),
    Context = [source_file('examples/stage8.pl'), applied_passes([simplification, code_generation])],
    starlog:npl_ir_to_annotated_source_text(IR, Context, TextA),
    starlog:npl_ir_to_annotated_source_text(IR, Context, TextB),
    assertion(TextA == TextB).

test(stage8_regression_poly_identity_simplifies_to_variable) :-
    IR = lowered_ir([lowered_poly_eval(x, [0,1], y)]),
    starlog:npl_ir_to_annotated_source_text(IR, [], Text),
    assertion(sub_string(Text, _, _, _, 'y is x.')).

test(stage8_regeneration_keeps_control_flow_like_loop_node) :-
    IR = lowered_ir([direct_index_rule(index_spec([i]), [x-(i+3), y-(i+4)], result_collector(values))]),
    starlog:npl_ir_to_annotated_source_text(IR, [], Text),
    assertion(sub_string(Text, _, _, _, 'npl_generated_loop([i],[assign(x,i+3),assign(y,i+4)],collect(values)).')).

test(stage8_file_output_roundtrip_for_text_generation) :-
    tmp_file_stream(text, File, Stream),
    close(Stream),
    IR = lowered_ir([
        lowered_poly_eval(n, [0,1,1], sum_n),
        lowered_passthrough(ir_memo_site(cache_key, hit))
    ]),
    Context = [optimisation_report([gaussian_elimination, simplification])],
    starlog:npl_ir_to_annotated_source_file(IR, Context, File),
    setup_call_cleanup(
        open(File, read, In),
        read_string(In, _, FileText),
        close(In)
    ),
    assertion(sub_string(FileText, _, _, _, '%% optimisation report: [gaussian_elimination,simplification]')),
    assertion(sub_string(FileText, _, _, _, 'sum_n is n^2+n.')),
    assertion(sub_string(FileText, _, _, _, 'npl_generated_passthrough(ir_memo_site(cache_key,hit)).')),
    delete_file(File).

:- end_tests(pr3_stage8_regeneration_stability).
