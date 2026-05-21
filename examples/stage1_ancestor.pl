% Stage 1 sample program for the browser child Prolog engine.

parent(alice,bob).
parent(bob,charlie).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
