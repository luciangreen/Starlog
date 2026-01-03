% demo_find_template_evaluation.pl
% Demonstration of find/2 with template evaluation
% Shows how templates containing Starlog expressions are automatically evaluated

:- use_module(starlog).

demo_find_template_evaluation :-
    write('==================================================================='), nl,
    write('Demonstration of find/2 with Template Evaluation'), nl,
    write('==================================================================='), nl, nl,
    
    % Example 1: The exact problem statement
    write('Example 1: Problem Statement'), nl,
    write('  Query: Result is find(A:C, starlog_call([A:d:a:C] is [a:d:a:c]))'), nl,
    starlog_call(Result1 is find(A1:C1, starlog_call([A1:d:a:C1] is [a:d:a:c]))),
    format('  Result: ~q~n', [Result1]),
    format('  Explanation: A=~q, C=~q, so A:C evaluates to ~q~n', [A1, C1, Result1]),
    nl,
    
    % Example 2: String concatenation template
    write('Example 2: String Concatenation Template'), nl,
    write('  Query: Result is find(X:Y, starlog_call([X:"-":Y] is ["hello":"-":"world"]))'), nl,
    starlog_call(Result2 is find(X2:Y2, starlog_call([X2:"-":Y2] is ["hello":"-":"world"]))),
    format('  Result: ~q~n', [Result2]),
    format('  Explanation: X=~q, Y=~q, so X:Y evaluates to ~q~n', [X2, Y2, Result2]),
    nl,
    
    % Example 3: Atom concatenation template
    write('Example 3: Atom Concatenation Template'), nl,
    write('  Query: Result is find(A•C, starlog_call([A•x•C] is [y•x•z]))'), nl,
    starlog_call(Result3 is find(A3•C3, starlog_call([A3•x•C3] is [y•x•z]))),
    format('  Result: ~q~n', [Result3]),
    format('  Explanation: A=~q, C=~q, so A•C evaluates to ~q~n', [A3, C3, Result3]),
    nl,
    
    % Example 4: Multiple variable template
    write('Example 4: Multiple Variables in Template'), nl,
    write('  Query: Result is find(A:"-":C, starlog_call([A:"-":C] is ["x":"-":"y"]))'), nl,
    starlog_call(Result4 is find(A4:"-":C4, starlog_call([A4:"-":C4] is ["x":"-":"y"]))),
    format('  Result: ~q~n', [Result4]),
    format('  Explanation: A=~q, C=~q, so A:"-":C evaluates to ~q~n', [A4, C4, Result4]),
    nl,
    
    % Example 5: Contrast with list template (not evaluated)
    write('Example 5: List Template (for comparison)'), nl,
    write('  Query: Result is find([A,C], starlog_call([A:x:C] is [p:x:q]))'), nl,
    starlog_call(Result5 is find([A5,C5], starlog_call([A5:x:C5] is [p:x:q]))),
    format('  Result: ~q~n', [Result5]),
    format('  Explanation: Template is a list, so it is NOT evaluated~n'),
    format('  A=~q, C=~q, result is the list [A,C] = ~q~n', [A5, C5, Result5]),
    nl,
    
    % Example 6: Plain variable template (not evaluated)
    write('Example 6: Plain Variable Template (for comparison)'), nl,
    write('  Query: Result is find(X, member(X, [1,2,3]))'), nl,
    starlog_call(Result6 is find(X6, member(X6, [1,2,3]))),
    format('  Result: ~q~n', [Result6]),
    format('  Explanation: Template is a plain variable, so it is NOT evaluated~n'),
    nl,
    
    write('==================================================================='), nl,
    write('Key Points:'), nl,
    write('- Templates with Starlog operators (:, &, •) are automatically evaluated'), nl,
    write('- The evaluation happens after the goal succeeds and variables are bound'), nl,
    write('- This allows you to collect computed results directly'), nl,
    write('- List templates and plain variables are not evaluated (backward compatible)'), nl,
    write('- find/2 uses Starlog syntax: Result is find(Template, Goal)'), nl,
    write('==================================================================='), nl.

:- initialization(demo_find_template_evaluation, main).
