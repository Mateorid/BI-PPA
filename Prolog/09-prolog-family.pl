% ======= Databaze faktu ========
man(bart).
man(homer).
man(abraham).
man(clancy).
woman(mona).
woman(marge).
woman(liza).
woman(meggie).
woman(selma).
woman(patty).
woman(jacqueline).
parent(homer,bart).
parent(homer,liza).
parent(homer,meggie).
parent(abraham,homer).
parent(marge,bart).
parent(marge,liza).
parent(marge,meggie).
parent(mona,homer).
parent(jacqueline,marge).
parent(jacqueline,patty).
parent(jacqueline,selma).
married(homer,marge).
married(abraham,mona).
married(clancy,jacqueline).

% father(F,X) : man(F) and parent (F,X).
father(F,X) :- man(F), parent(F,X).
son(S,X) :- man(S), parent(X,S).
grandparent(G,X) :- parent(G,Y), parent(Y,X).

ancestor(A, X) :- parent(A, X).
ancestor(A, X) :- parent(A, Y), ancestor(Y, X).


sibling(X,Y) :- parent(Z, X), parent(Z,Y), X \= Y.

% USECKA.
svisla(usecka(bod(X,Py), bod(X,Qy))).

% fib(+N, -Res).
fib(0,0).
fib(1,1).
fib(N, Res) :- 
	N2 is N - 2,
	N1 is N - 1,
	fib(N2, Res2),
	fib(N1, Res1),
	Res is Res2 + Res1.
