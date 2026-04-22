:- module(pr3_stage6_tests, [run_pr3_stage6_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr3_stage6_tests :-
    run_tests([pr3_stage6_annotated_source_regeneration]).

:- begin_tests(pr3_stage6_annotated_source_regeneration).

test(stage6_predicates_exist) :-
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_text/3)),
    assertion(current_predicate(starlog:npl_ir_to_annotated_source_file/3)).

test(stage6_annotated_text_includes_context_and_generated_statements) :-
    IR = lowered_ir([
        lowered_poly_eval(i, [0,1], poly_result),
        direct_index_rule(index_spec([i]), [x-(i+3)], result_collector(values))
    ]),
    Context = [
        source_file('examples/lists.pl'),
        recursion_classification(linear),
        optimisation_report([gaussian_elimination, direct_index_rule_reconstruction])
    ],
    starlog:npl_ir_to_annotated_source_text(IR, Context, Text),
    assertion(sub_string(Text, _, _, _, '%% original source file: examples/lists.pl')),
    assertion(sub_string(Text, _, _, _, '%% recursion classification: linear')),
    assertion(sub_string(Text, _, _, _, '%% optimisation report: [gaussian_elimination,direct_index_rule_reconstruction]')),
    assertion(sub_string(Text, _, _, _, 'poly_result is i.')),
    assertion(sub_string(Text, _, _, _, 'npl_generated_loop([i],[assign(x,i+3)],collect(values)).')).

test(stage6_annotated_text_marks_special_ir_nodes) :-
    IR = lowered_ir([
        lowered_passthrough(ir_memo_site(cache_key, hit)),
        lowered_passthrough(ir_addr_loop(loop_id, [i])),
        lowered_passthrough(ir_source_marker('examples/lists.pl', line(12)))
    ]),
    starlog:npl_ir_to_annotated_source_text(IR, [], Text),
    assertion(sub_string(Text, _, _, _, '%% memoisation marker present in IR')),
    assertion(sub_string(Text, _, _, _, '%% address-loop marker present in IR')),
    assertion(sub_string(Text, _, _, _, '%% source-marker node present in IR')),
    assertion(sub_string(Text, _, _, _, 'npl_generated_passthrough(ir_memo_site(cache_key,hit)).')).

test(stage6_annotated_file_output_writes_regenerated_text) :-
    tmp_file_stream(text, File, Stream),
    close(Stream),
    IR = lowered_ir([lowered_poly_eval(i, [0,1,1], poly_result)]),
    Context = [source_metadata(meta([origin(test)])), applied_passes([simplification, code_generation])],
    starlog:npl_ir_to_annotated_source_file(IR, Context, File),
    setup_call_cleanup(
        open(File, read, In),
        read_string(In, _, FileText),
        close(In)
    ),
    assertion(sub_string(FileText, _, _, _, '%% source metadata: meta([origin(test)])')),
    assertion(sub_string(FileText, _, _, _, '%% applied optimisation passes: [simplification,code_generation]')),
    assertion(sub_string(FileText, _, _, _, 'poly_result is i^2+i.')),
    delete_file(File).

:- end_tests(pr3_stage6_annotated_source_regeneration).
