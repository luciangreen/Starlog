% Test for proper spacing in 'is' operator display
% This test verifies the fix for the issue where expressions like
% "aa" is a:a were displayed without space as "aa"is a:a

:- use_module('../starlog').

:- begin_tests(portray_spacing).

test(string_is_operator_spacing, []) :-
    % The problem from the issue:
    % ?- A=(B is a:a),true,starlog_call(A).
    % Expected: A = ("aa" is a:a),
    % Previously showed: A = ("aa"is a:a),
    A=(B is a:a),
    true,
    starlog_call(A),
    % Verify B gets the correct value
    B = "aa",
    % Verify that when printed with print/1, there's proper spacing
    with_output_to(string(Output), print(A)),
    % The output should contain space between "aa" and is
    sub_string(Output, _, _, _, "\"aa\" is a:a").

test(atom_is_operator_spacing, []) :-
    % Test with atoms too
    A=(B is foo:bar),
    starlog_call(A),
    B = "foobar",
    % Verify proper display with atoms
    with_output_to(string(Output), print(A)),
    sub_string(Output, _, _, _, "\"foobar\" is ").

test(number_is_operator_no_portray, []) :-
    % Numbers shouldn't trigger our portray hook
    % (they don't need special spacing treatment)
    A=(B is 1+1),
    A,
    B = 2,
    % This shouldn't use our custom portray
    with_output_to(string(Output), print(A)),
    % Just verify it contains 'is'
    sub_string(Output, _, _, _, "is").

:- end_tests(portray_spacing).

% Helper to run tests
run_tests :-
    writeln('Running portray spacing tests...'),
    run_tests(portray_spacing),
    writeln('Portray spacing tests complete!').
