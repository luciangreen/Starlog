:- module(pr2_stage12_tests, [run_pr2_stage12_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage12_tests :-
    run_tests([pr2_stage12_safety_and_correctness]).

:- begin_tests(pr2_stage12_safety_and_correctness).

test(stage12_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage12_must_transform_when/1)),
    assertion(current_predicate(starlog:npl_stage12_must_not_transform_when/1)),
    assertion(current_predicate(starlog:npl_stage12_transform_decision/2)).

test(stage12_must_transform_conditions_are_explicit) :-
    starlog:npl_stage12_must_transform_when(Conditions),
    assertion(Conditions == [
        variable_correspondences_explicit,
        independent_variables_identifiable,
        predicates_reduce_to_pattern_matching_plus_irreducible_commands,
        polynomial_formulas_solved_by_gaussian_elimination_where_applicable,
        validation_succeeds,
        observable_behaviour_preserved,
        eliminated_intermediate_structures_not_needed_elsewhere
    ]).

test(stage12_must_not_transform_conditions_are_explicit) :-
    starlog:npl_stage12_must_not_transform_when(Conditions),
    assertion(Conditions == [
        variable_identity_ambiguous,
        predicates_impure,
        reduction_to_supported_primitives_failed,
        index_relationships_not_cleanly_reconstructed,
        polynomial_validation_failed,
        output_multiplicity_or_ordering_changes_unjustified,
        full_structure_materialisation_observably_required
    ]).

test(stage12_transform_decision_accepts_when_all_required_conditions_hold) :-
    starlog:npl_stage12_transform_decision([
        variable_correspondences_explicit-true,
        independent_variables_identifiable-true,
        predicates_reduce_to_pattern_matching_plus_irreducible_commands-true,
        polynomial_formulas_solved_by_gaussian_elimination_where_applicable-true,
        validation_succeeds-true,
        observable_behaviour_preserved-true,
        eliminated_intermediate_structures_not_needed_elsewhere-true
    ], Decision),
    assertion(Decision == allow_transform).

test(stage12_transform_decision_rejects_on_ambiguous_identity_even_if_required_checks_are_true) :-
    starlog:npl_stage12_transform_decision([
        variable_correspondences_explicit-true,
        independent_variables_identifiable-true,
        predicates_reduce_to_pattern_matching_plus_irreducible_commands-true,
        polynomial_formulas_solved_by_gaussian_elimination_where_applicable-true,
        validation_succeeds-true,
        observable_behaviour_preserved-true,
        eliminated_intermediate_structures_not_needed_elsewhere-true,
        variable_identity_ambiguous-true
    ], Decision),
    assertion(Decision == reject(variable_identity_ambiguous)).

test(stage12_transform_decision_rejects_when_full_structure_is_required) :-
    starlog:npl_stage12_transform_decision([
        variable_correspondences_explicit-true,
        independent_variables_identifiable-true,
        predicates_reduce_to_pattern_matching_plus_irreducible_commands-true,
        polynomial_formulas_solved_by_gaussian_elimination_where_applicable-true,
        validation_succeeds-true,
        observable_behaviour_preserved-true,
        eliminated_intermediate_structures_not_needed_elsewhere-false,
        full_structure_materialisation_observably_required-true
    ], Decision),
    assertion(Decision == reject(full_structure_materialisation_observably_required)).

:- end_tests(pr2_stage12_safety_and_correctness).
