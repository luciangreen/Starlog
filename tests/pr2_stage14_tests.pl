:- module(pr2_stage14_tests, [run_pr2_stage14_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage14_tests :-
    run_tests([pr2_stage14_delivery_rules]).

:- begin_tests(pr2_stage14_delivery_rules).

test(stage14_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage14_delivery_order/1)),
    assertion(current_predicate(starlog:npl_stage14_additional_delivery_rules/1)),
    assertion(current_predicate(starlog:npl_stage14_concise_agent_prompt/1)).

test(stage14_delivery_order_is_explicit_and_deterministic) :-
    starlog:npl_stage14_delivery_order(OrderA),
    starlog:npl_stage14_delivery_order(OrderB),
    assertion(OrderA == OrderB),
    assertion(OrderA == [
        audit_current_implementation,
        add_failing_tests_first,
        implement_gaussian_elimination_as_universal_polynomial_solver,
        integrate_gaussian_elimination_into_recurrence_optimisation,
        implement_general_indexed_variable_tracing_from_first_principles,
        implement_formula_reconstruction_for_indexed_variables,
        use_gaussian_elimination_for_indexed_polynomial_formulas,
        add_ir_and_codegen_support,
        update_examples_and_comments,
        update_docs,
        run_tests_and_self_check,
        document_unsupported_cases
    ]).

test(stage14_additional_delivery_rules_are_explicit) :-
    starlog:npl_stage14_additional_delivery_rules(Rules),
    assertion(Rules == [
        do_not_upload_files_without_meaningful_code_or_docs_changes,
        do_not_guess_coefficients,
        do_not_insert_hardcoded_closed_forms_as_discovery_shortcuts,
        do_not_build_optimiser_around_named_structural_cases,
        do_not_claim_generality_without_proving_reducibility_and_equivalence,
        prefer_smaller_correct_passes_over_broad_unverifiable_rewrites
    ]).

test(stage14_concise_prompt_matches_required_shorthand) :-
    starlog:npl_stage14_concise_agent_prompt(Prompt),
    assertion(Prompt == 'derive affine and polynomial formulas for indexed variables, such as x_i=4+i-1 and y_i=5+i-1, from traced variable correspondences.').

:- end_tests(pr2_stage14_delivery_rules).
