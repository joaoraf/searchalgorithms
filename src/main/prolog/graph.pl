:- module(graph,[
		reorder_edges/5,
		compare_vertices/4,
		compare_edges/4,
		compare_map/5,
		reverse_order/4,
		compare_compose/5
	]).

:- meta_predicate reorder_edges(3,3,?,?,?).

reorder_edges(OrderPred,Rel,P1,Edge,P2) :-
	aggregate(set([E,V]),call(Rel,P1,E,V),Unsorted),
	predsort(OrderPred,Unsorted,Sorted),
	member([Edge,P2],Sorted).
	
:- meta_predicate compare_vertices(3,-,+,+),
		  compare_edges(3,-,+,+),
		  compare_map(2,3,-,+,+),
		  reverse_order(3,-,+,+).
	
compare_vertices(OrderPred,Res,[V1,_],[V2,_]) :-
	call(OrderPred,Res,V1,V2).
compare_edges(OrderPred,Res,[_,E1],[_,E2]) :-
	call(OrderPred,Res,E1,E2).
compare_map(MapPred,OrderPred,Res,X,Y) :-
	call(MapPred,X,X1),
	call(MapPred,Y,Y1),
	call(OrderPred,Res,X1,Y1).
	
reverse_order(OrderPred,Res,X,Y) :-
	call(OrderPred,Res,Y,X).
	
compare_compose(OrderPred1,OrderPred2,Res,X,Y) :-
	call(OrderPred1,Res1,X,Y),
	(Res1 = (=) -> call(OrderPred2,Res,X,Y) ; Res = Res1).