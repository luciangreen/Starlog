:- module(pr2_stage13_tests, [run_pr2_stage13_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage13_tests :-
    run_tests([pr2_stage13_self_hosting_alignment]).

:- begin_tests(pr2_stage13_self_hosting_alignment).

test(stage13_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage13_toggleable_passes/1)),
    assertion(current_predicate(starlog:npl_stage13_effective_pass_toggles/2)),
    assertion(current_predicate(starlog:npl_stage13_rebuild_log/3)),
    assertion(current_predicate(starlog:npl_stage13_formula_provenance/2)),
    assertion(current_predicate(starlog:npl_stage13_self_hosting_invariants/1)),
    assertion(current_predicate(starlog:npl_stage13_self_check/2)).

test(stage13_passes_are_toggleable_and_deterministic) :-
    starlog:npl_stage13_toggleable_passes(PassesA),
    starlog:npl_stage13_toggleable_passes(PassesB),
    assertion(PassesA == PassesB),
    assertion(PassesA == [
        pass(gaussian_elimination_polynomial_solve, true),
        pass(gaussian_elimination_indexed_polynomial_formula_solve, true),
        pass(direct_index_rule_reconstruction, true),
        pass(code_generation, true)
    ]).

test(stage13_overrides_apply_without_dropping_other_defaults) :-
    starlog:npl_stage13_effective_pass_toggles(
        [pass(code_generation, false)],
        Effective),
    assertion(member(pass(code_generation, false), Effective)),
    assertion(member(pass(gaussian_elimination_polynomial_solve, true), Effective)),
    assertion(member(pass(direct_index_rule_reconstruction, true), Effective)).

test(stage13_rebuild_log_records_applied_transforms) :-
    Transforms = [
        transform(gaussian_elimination_polynomial_solve, applied),
        transform(direct_index_rule_reconstruction, skipped(disabled))
    ],
    starlog:npl_stage13_rebuild_log(
        Transforms,
        [provenance(stage13_trace_lineage)],
        Log),
    assertion(Log == rebuild_log(
        [
            transform(gaussian_elimination_polynomial_solve, applied),
            transform(direct_index_rule_reconstruction, skipped(disabled))
        ],
        meta([provenance(stage13_trace_lineage)])
    )).

test(stage13_formula_provenance_is_inspectable) :-
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [i],
                                [x-(4+i-1), y-(5+i-1)],
                                [0,1,1],
                                [provenance(stage13_formula_trace)],
                                IR),
    starlog:npl_stage13_formula_provenance(IR, Provenance),
    assertion(Provenance == [stage13_formula_trace]).

test(stage13_self_check_reports_clean_status_for_valid_ir) :-
    starlog:npl_stage8_build_ir(flow_graph(mock, map([]), [], trace([])),
                                [i],
                                [x-(4+i-1), y-(5+i-1)],
                                [0,1,1],
                                [provenance(stage13_self_check)],
                                IR),
    starlog:npl_stage13_self_check(IR, Report),
    Report = self_check_report(pass, Invariants, Checks),
    assertion(member(invariant(self_check_runs_cleanly, pass), Invariants)),
    assertion(member(invariant(rebuild_pipeline_operational, pass), Invariants)),
    assertion(member(invariant(optimisation_metadata_parser_ir_codegen_compatible, pass), Invariants)),
    assertion(member(check(stage8_ir_metadata_available, pass), Checks)),
    assertion(member(check(stage9_codegen_compatible, pass), Checks)).

:- end_tests(pr2_stage13_self_hosting_alignment).
