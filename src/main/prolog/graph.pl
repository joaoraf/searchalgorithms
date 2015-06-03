:- module(graph,[
		reorder_edges/5,
		compare_vertices/4,
		compare_edges/4,
		compare_paths/4,
		compare_map/5,
		reverse_order/4,
		compare_compose/5,
		compare_path_lengths/3
	]).

:- meta_predicate reorder_edges(3,3,?,?,?).

reorder_edges(OrderPred,Rel,P1,Edge,P2) :-
	(var(P1) *-> (
		aggregate(set(V),E^V1^call(Rel,V,E,V1),Vs),		
		member(P1,Vs)
	 ) ; true),	
	findall([E,V],call(Rel,P1,E,V),Unsorted),
	predsort(OrderPred,Unsorted,Sorted),	
	member([Edge,P2],Sorted).
	
:- meta_predicate compare_vertices(3,-,+,+),
		  compare_edges(3,-,+,+),
		  compare_map(2,3,-,+,+),
		  reverse_order(3,-,+,+).
	
compare_vertices(OrderPred,Res,[_,V1],[_,V2]) :-
	call(OrderPred,Res,V1,V2).
compare_edges(OrderPred,Res,[E1,_],[E2,_]) :-
	call(OrderPred,Res,E1,E2).
compare_paths(OrderPred,Res,[_,P1],[_,P2]) :-
	call(OrderPred,Res,P1,P2).	
compare_map(MapPred,OrderPred,Res,X,Y) :-
	call(MapPred,X,X1),
	call(MapPred,Y,Y1),
	call(OrderPred,Res,X1,Y1).
	
reverse_order(OrderPred,Res,X,Y) :-
	call(OrderPred,Res,Y,X).
	
compare_compose(OrderPred1,OrderPred2,Res,X,Y) :-
	call(OrderPred1,Res1,X,Y),
	(Res1 = (=) -> call(OrderPred2,Res,X,Y) ; Res = Res1).

compare_path_lengths(Res,[_,P1],[_,P2]) :- 
	length(P1,N1),
	length(P2,N2),
	compare(Res,N1,N2).	