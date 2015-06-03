:- module(map,[
	edge_rel_from_map/3,
	normalize_moves/2,
	as_birds_fly/3,
	distance_bounds/3,
	distance_compare/4,
	reorder_by_distance/5,
	moves_cost/2
	]).

:- use_module(library(clpfd)).

edge_rel_from_map(Dirs,Map,EdgeRel) :-
	length(Map,Height),
	aggregate(min(W),(member(R,Map),length(R,W)),Width),
	EdgeRel=map:map_edge_rel(Dirs,Map,Height,Width).

cell(Map,Height,Width,[X,Y],C) :-
	X #>= 0,
	X #<  Width,
	Y #>= 0,
	Y #<  Height,
	label([X,Y]),
	nth0(Y,Map,Row),
	nth0(X,Row,C).

map_edge_rel(Dirs,Map,Height,Width,P1,Move,P2) :-
	P1 = [_,_],
	P2 = [_,_],
	positions(Dirs,Height,Width,P1,Move,P2),
	cell(Map,Height,Width,P1,C1),
	C1 \= w,
	cell(Map,Height,Width,P2,C2),
	C2 \= w
	.

positions(Dirs,Height,Width,[X1,Y1],Move,[X2,Y2]) :-
%	format('positions(Dirs=~p,Height=~p,Width=~p,P1=~p,Move=~p,P2=~p)\n',
%		[Dirs,Height,Width,P1,Move,P2]),
	X1 #>= 0, X1 #< Width,
	Y1 #>= 0, Y1 #< Height,
	X2 #>= 0, X2 #< Width,
	Y2 #>= 0, Y2 #< Height,
	DX #= X2 - X1,
	DY #= Y2 - Y1,
	DX #>= -1,
	DX #=< 1,
	DY #>= -1,
	DY #=< 1,
	(DX #\= 0 #\/ DY #\= 0),				
	label([X1,Y1,X2,Y2,DX,DY]),
	move(Dirs,DX,DY,Move).

move(_, 0, 1,s) :- !.
move(_, 0,-1,n) :- !.
move(_, 1, 0,e) :- !.
move(_,-1, 0,w) :- !.
move(8, 1, 1,se) :- !.
move(8, 1, -1,ne) :- !.
move(8, -1, 1,sw) :- !.
move(8, -1, -1,nw) :- !.

normalize_moves(Moves,CompMoves) :- normalize_moves(Moves,[],CompMoves).	
normalize_moves([],MS,CM) :- reverse(MS,CM).
normalize_moves([M|MS],RM,CM) :-
	functor(M,MF,1),
	arg(1,M,1),
	normalize_moves([MF|MS],RM,CM).
normalize_moves([M|MS],RM,CM) :-
	functor(M,_,1),
	arg(1,M,0),
	normalize_moves(MS,RM,CM).	
normalize_moves([M1|[M2|MS]],RM,CM) :-
	functor(M1,M,A1),
	functor(M2,M,A2),
	(A1 = 1 -> arg(1,M1,N1) ; N1 = 1),
	(A2 = 1 -> arg(1,M2,N2) ; N2 = 1),
	N3 is N1 + N2,
	functor(M3,M,1),
	arg(1,M3,N3),
	normalize_moves([M3|MS],RM,CM).
normalize_moves([M1|[M2|MS]],RM,CM) :-
	functor(M1,F1,_),
	functor(M2,F2,_),
	F1 \= F2,	
	normalize_moves([M2|MS],[M1|RM],CM).
normalize_moves([M],RM,CM) :-
	normalize_moves([],[M|RM],CM).		

moves_cost(MS,C) :- moves_cost(MS,0,C).
moves_cost([], C,C).
moves_cost([M|MS],C,CF) :-
	functor(M,_,1),
	arg(1,M,N),
	C1 is C + N,
	moves_cost(MS,C1,CF).
moves_cost([M|MS],C,CF) :-
	functor(M,_,0),	
	C1 is C + 1,
	moves_cost(MS,C1,CF).	

as_birds_fly([X1,Y1],[X2,Y2],D) :-
	DX is X1 - X2,
	DY is Y1 - Y2,
	D is sqrt(DX*DX + DY*DY).	

distance_bounds(V,[P,V],solution(D)) :-	
	length(P,D),!.
distance_bounds(Ref,P,lower_bound(D)) :-
	total_distance(Ref,P,D),!.
		
total_distance(V1,[P,V],D) :-
	length(P,D1),
	as_birds_fly(V,V1,D2),
	D is D1 + D2.
	
distance_compare(Ref,Res,P1,P2) :-
	graph:compare_vertices(graph:compare_map(map:as_birds_fly(Ref),compare),Res,P1,P2).

:- meta_predicate reorder_by_distance(+,3,?,?,?).

reorder_by_distance(Ref,Rel,P1,E,P2) :- 
	graph:reorder_edges(graph:compare_compose(map:distance_compare(Ref),compare),Rel,P1,E,P2).