:- module(sample_graphs, [
		circle_graph/1,
		circle/2,
		circle_g/3
	]).
	
:- use_module(library(clpfd)).

:- use_module(graph).
		
divisor(X,Y) :-
	X #> 0,
	Y #> 1,
	Y #< X,
	X mod Y #= 0,	
	label([X,Y]).
			
div_relation(X,Ys) :- aggregate(set(Y),divisor(X,Y),Ys),!.
div_relation(_X,Ys) :- Ys = [],!.

circle_cons([X1,Y1],[X2,Y2]) :-
	(X1 * X1 + Y1 * Y1) #= (X2 * X2 + Y2 * Y2),
	(X1 #\= X2) #\/  (Y1 #\= Y2).

circle([X1,Y1],[X2,Y2]) :- circle_cons([X1,Y1],[X2,Y2]),label([X1,Y1,X2,Y2]).

circle_relation([X,Y],Ys) :-
	aggregate(set([X1,Y1]),circle([X,Y],[X1,Y1]),Ys).

circle_g(P1,'',P2) :- circle(P1,P2).

circle_graph(G) :-
	G = _{ edge: circle_g}. 	