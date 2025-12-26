% demo_call_to_variable.pl
% Demonstration of saving Prolog and Starlog calls to variables

:- use_module(starlog_in_prolog).

% Example 1: Using starlog_call/2 to save results
demo_starlog_call_2 :-
    write('Example 1: Using starlog_call/2 to save results'), nl,
    starlog_call(X is "Hello":"World", Greeting),
    write('  starlog_call(X is "Hello":"World", Greeting)'), nl,
    write('  Greeting = '), write(Greeting), nl,
    write('  (Result explicitly saved to Greeting variable)'), nl, nl.

% Example 2: Using starlog_eval/2 to evaluate expressions
demo_starlog_eval :-
    write('Example 2: Using starlog_eval/2 to evaluate expressions'), nl,
    
    % String concatenation
    starlog_eval("x":"y":"z", Result1),
    write('  starlog_eval("x":"y":"z", Result1)'), nl,
    write('  Result1 = '), write(Result1), nl,
    
    % Arithmetic
    starlog_eval(5 + 3, Result2),
    write('  starlog_eval(5 + 3, Result2)'), nl,
    write('  Result2 = '), write(Result2), nl,
    
    % List operations
    starlog_eval([1,2] & [3,4], Result3),
    write('  starlog_eval([1,2] & [3,4], Result3)'), nl,
    write('  Result3 = '), write(Result3), nl,
    nl.

% Example 3: Using starlog_no_eval/2 to preserve expressions
demo_starlog_no_eval :-
    write('Example 3: Using starlog_no_eval/2 to preserve expressions'), nl,
    
    % Preserve arithmetic
    starlog_no_eval(1+1, Expr1),
    write('  starlog_no_eval(1+1, Expr1)'), nl,
    write('  Expr1 = '), write(Expr1), nl,
    write('  (Expression preserved, not evaluated)'), nl,
    
    % Preserve string concatenation
    starlog_no_eval("hello":"world", Expr2),
    write('  starlog_no_eval("hello":"world", Expr2)'), nl,
    write('  Expr2 = '), write(Expr2), nl,
    write('  (Starlog operator preserved)'), nl,
    
    % Preserve complex expression
    starlog_no_eval((x+y)*(a+b), Expr3),
    write('  starlog_no_eval((x+y)*(a+b), Expr3)'), nl,
    write('  Expr3 = '), write(Expr3), nl,
    write('  (Complex expression preserved as data)'), nl,
    nl.

% Example 4: Comparison - eval vs no_eval
demo_comparison :-
    write('Example 4: Comparison - eval vs no_eval'), nl,
    
    starlog_eval("a":"b", Evaluated),
    starlog_no_eval("a":"b", Preserved),
    
    write('  starlog_eval("a":"b", Evaluated)'), nl,
    write('  Evaluated = '), write(Evaluated), nl,
    
    write('  starlog_no_eval("a":"b", Preserved)'), nl,
    write('  Preserved = '), write(Preserved), nl,
    
    write('  (Shows difference between evaluation and preservation)'), nl, nl.

% Example 5: Building formulas as data
demo_formula_building :-
    write('Example 5: Building formulas as data'), nl,
    
    % Store formula without evaluating
    starlog_no_eval(x*2 + y, Formula),
    write('  starlog_no_eval(x*2 + y, Formula)'), nl,
    write('  Formula = '), write(Formula), nl,
    write('  (Useful for symbolic computation or meta-programming)'), nl, nl.

% Example 6: Chaining operations with starlog_call/2
demo_chaining :-
    write('Example 6: Chaining operations with starlog_call/2'), nl,
    
    starlog_call(A is "First":"Part", R1),
    write('  Step 1: starlog_call(A is "First":"Part", R1)'), nl,
    write('  R1 = '), write(R1), nl,
    
    starlog_call(B is R1:" Complete", R2),
    write('  Step 2: starlog_call(B is R1:" Complete", R2)'), nl,
    write('  R2 = '), write(R2), nl,
    nl.

% Example 7: Controlled evaluation with eval in no_eval
demo_controlled_eval :-
    write('Example 7: Controlled evaluation with eval in no_eval'), nl,
    
    % Partially evaluate
    starlog_call(X is no_eval("Result: " : eval("a":"b")), Result),
    write('  starlog_call(X is no_eval("Result: " : eval("a":"b")), Result)'), nl,
    write('  Result = '), write(Result), nl,
    write('  (Only the eval part is evaluated)'), nl, nl.

% Example 8: Practical use case - template system
demo_template :-
    write('Example 8: Practical use case - template system'), nl,
    
    % Store template
    starlog_no_eval("Welcome, " : username : "!", Template),
    write('  starlog_no_eval("Welcome, " : username : "!", Template)'), nl,
    write('  Template = '), write(Template), nl,
    write('  (Template stored for later use with actual username)'), nl, nl.

% Run all demonstrations
run_demos :-
    write('=============================================='), nl,
    write('Saving Calls to Variables - Demonstration'), nl,
    write('=============================================='), nl, nl,
    demo_starlog_call_2,
    demo_starlog_eval,
    demo_starlog_no_eval,
    demo_comparison,
    demo_formula_building,
    demo_chaining,
    demo_controlled_eval,
    demo_template,
    write('=============================================='), nl,
    write('Demonstration complete!'), nl,
    write('=============================================='), nl.

:- initialization(run_demos, main).
