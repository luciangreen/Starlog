:- module(pr2_stage11_tests, [run_pr2_stage11_tests/0]).

:- use_module(library(plunit)).
:- use_module('../starlog').

run_pr2_stage11_tests :-
    run_tests([pr2_stage11_documentation_rewrite]).

:- begin_tests(pr2_stage11_documentation_rewrite).

test(stage11_predicates_exist) :-
    assertion(current_predicate(starlog:npl_stage11_documentation_scope/1)),
    assertion(current_predicate(starlog:npl_stage11_gaussian_recursion_requirements/1)),
    assertion(current_predicate(starlog:npl_stage11_optimisation_rule_categories/1)),
    assertion(current_predicate(starlog:npl_stage11_example_requirements/1)),
    assertion(current_predicate(starlog:npl_stage11_readme_requirements/1)).

test(stage11_documentation_scope_lists_required_surfaces) :-
    starlog:npl_stage11_documentation_scope(Scope),
    assertion(member(gaussian_recursion_md('GAUSSIAN_RECURSION.md'), Scope)),
    assertion(member(optimisation_rules_md('OPTIMISATION_RULES.md'), Scope)),
    assertion(member(readme_md('README.md'), Scope)),
    assertion(member(example_files([examples_pl, demo_problem_statement_complete_pl]), Scope)).

test(stage11_gaussian_recursion_requirements_are_explicit) :-
    starlog:npl_stage11_gaussian_recursion_requirements(Reqs),
    assertion(member(gaussian_elimination_mandatory_for_polynomial_discovery, Reqs)),
    assertion(member(finite_differences_only_for_degree_estimation, Reqs)),
    assertion(member(triangular_number_worked_example_documented, Reqs)),
    assertion(member(indexed_variable_polynomial_discovery_documented, Reqs)),
    assertion(member(unsupported_cases_documented, Reqs)).

test(stage11_optimisation_rules_distinguish_required_categories) :-
    starlog:npl_stage11_optimisation_rule_categories(Categories),
    assertion(Categories == [
        accumulator_rewrites,
        polynomial_discovery_via_gaussian_elimination,
        general_indexed_variable_tracing,
        reduction_to_pattern_matching_plus_irreducibles,
        formula_reconstruction_from_first_principles,
        indexed_variable_formula_derivation,
        direct_index_rule_generation
    ]).

test(stage11_examples_require_unoptimised_vs_optimised_and_index_lineage) :-
    starlog:npl_stage11_example_requirements(Reqs),
    assertion(member(show_unoptimised_and_optimised_versions, Reqs)),
    assertion(member(comment_variable_lineage_clearly, Reqs)),
    assertion(member(explain_index_assignment, Reqs)),
    assertion(member(explain_formula_reconstruction, Reqs)),
    assertion(member(include_nontrivial_indexed_case, Reqs)),
    assertion(member(include_indexed_variable_examples([x-(4+i-1), y-(5+i-1)]), Reqs)),
    assertion(member(avoid_named_special_case_framing, Reqs)).

test(stage11_readme_requirements_are_explicit) :-
    starlog:npl_stage11_readme_requirements(Reqs),
    assertion(member(wam_works, Reqs)),
    assertion(member(gaussian_elimination_used_for_polynomial_discovery, Reqs)),
    assertion(member(first_principles_symbolic_tracing_for_indexed_subterms, Reqs)),
    assertion(member(indexed_variable_formulas_reconstructed_from_tracing, Reqs)),
    assertion(member(named_structural_cases_not_hardcoded_design_basis, Reqs)),
    assertion(member(unsupported_or_ambiguous_cases_remain_unchanged, Reqs)).

:- end_tests(pr2_stage11_documentation_rewrite).
