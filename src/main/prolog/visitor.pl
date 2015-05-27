:- module(visitor,[
		make_visitor/2,
		make_visitor/3,
		make_decorated_visitor/5,
		expand_visitor/2,
		call_visitor//4,
		wrapping//2
	]).

call_visitor(Visitor,Vertice,Path,Action,In,Out) :-
	VisitorPred = Visitor.visitor_pred,
	call(VisitorPred,Vertice,Path,Action,In,Out).

:- meta_predicate wrapping(+,3,+,-).

wrapping(Visitor,Pred,In,Out) :-
	expand_visitor(Visitor,V1),
	Wrap = V1.wrap,	
	call(Wrap,In,State1),
	V2 = V1.put(_{ wrap: visitor:id_rel, unwrap: visitor:id_rel}),
	call(Pred,V2,State1,State2),
	Unwrap = V1.unwrap,
	call(Unwrap,State2,Out).
	
:- meta_predicate make_visitor(5,-).

make_visitor(VisitorPred,Visitor) :-	
	Visitor = visitor{		
		visitor_pred: VisitorPred,
		wrap: visitor:id_rel,
		unwrap: visitor:id_rel
	}.	

:- meta_predicate make_visitor(5,+,-).

make_visitor(VisitorPred,InitialState,Visitor) :-	
	Visitor = visitor{		
		visitor_pred: VisitorPred,
		wrap: visitor:const_rel(InitialState),
		unwrap: visitor:id_rel
	}.

:- meta_predicate make_decorated_visitor(+,-,5,+,-).

make_decorated_visitor(InitialState,FinalState, PreVisitorPred,VisitorExpr,DecoratedVisitor) :-
	expand_visitor(VisitorExpr,Visitor),	
	UnderVisitorPred = Visitor.visitor_pred,
	UnderWrap = Visitor.wrap,
	UnderUnwrap = Visitor.unwrap,
	DecoratedVisitor=visitor{		
		visitor_pred: visitor:decorated_visitor_pred(PreVisitorPred,UnderVisitorPred),		
		wrap: visitor:wrap_on_pair(InitialState,UnderWrap),
		unwrap: visitor:unwrap_from_pair(FinalState,UnderUnwrap)
	}.

decorated_visitor_pred(PreVisitorPred,VisitorPred,Vertice,Path,Action,[OverInState,UnderInState],[OverOutState,UnderOutState]) :-
	call(PreVisitorPred,Vertice,Path,Action1,OverInState,OverOutState),
	(Action1 = stop -> (Action = Action1, UnderOutState = UnderInState) ;
	   (Action1 = skip -> (Action = continue, UnderOutState = UnderInState) ;
	    call(VisitorPred,Vertice,Path,Action,UnderInState,UnderOutState))
	).  

:- dynamic wrap_count/1.
:- dynamic unwrap_count/1.

check_wrap :- wrap_count(_),!.
check_wrap :- assertz(wrap_count(0)),!.

inc_wrap :-
        check_wrap,
	wrap_count(X),!,
	X1 is X + 1,
	retractall(wrap_count(_)),
	assertz(wrap_count(X1)),
	format('wrap inc to: ~k\n',[X1]).
dec_wrap :-
	check_wrap,
	wrap_count(X),
	X1 is X - 1,
	retractall(wrap_count(_)),
	assertz(wrap_count(X1)),
	format('wrap dec to: ~k\n',[X1]).	

wrap_on_pair(OverState,UnderWrap,UnderState,ResultState) :-	
	call(UnderWrap,UnderState,WrappedState),
	ResultState = [OverState,WrappedState].
	

unwrap_from_pair(FinalState,UnderUnwrap,WrappedState,State) :-
	WrappedState = [OverState,UnderState],
	FinalState = OverState,	
	call(UnderUnwrap,UnderState,State).

const_rel(X,_Y,X).

id_rel(X,X).

expand_visitor(VisitorExpr,Visitor) :-
	is_dict(VisitorExpr,visitor),!,
	Visitor = VisitorExpr.
expand_visitor(VisitorExpr,Visitor) :-
	\+ is_dict(VisitorExpr,visitor),!,	
	expand_visitor_1(VisitorExpr,VisitorExpr1),
	expand_visitor(VisitorExpr1,Visitor).
	

:- use_module(library(ordsets)).

:- discontiguous expand_visitor_1/2.


expand_visitor_1(noop_visitor,V) :- make_visitor(null_visitor_pred,V).

null_visitor_pred(_V,_P,continue,XS,XS).

expand_visitor_1(collect_vertices(Result,Visitor),DecoratedVisitor) :-		 
	make_decorated_visitor([],Result,visitor:collect_vertices_pred,Visitor,DecoratedVisitor),!.	
	
collect_vertices_pred(V,_P,continue,XS,[V|XS]).

expand_visitor_1(collect_edge_path_to_vertices(Result,Visitor),DecoratedVisitor) :- 
	make_decorated_visitor([],Result,visitor:collect_edge_path_to_vertices_pred,Visitor,DecoratedVisitor),!.
	

collect_edge_path_to_vertices_pred(V,P,continue,XS,[[P2,V]|XS]) :-	
	(findall(E,member([_,E],P),P1) -> true ; P1 = []),
	reverse(P1,P2).
	
expand_visitor_1(collect_vertice_path_to_vertices(Result,Visitor),DecoratedVisitor) :-
	make_decorated_visitor([],Result,visitor:collect_vertice_path_to_vertices_pred,Visitor,DecoratedVisitor),!. 
	
collect_vertice_path_to_vertices_pred(V,P,continue,XS,[[P2,V]|XS]) :-		
	(findall(V1,member([V1,_],P),P1) -> true ; P1 = []),
	reverse(P1,P2).		

expand_visitor_1(collect_path_to_vertices(Result,Visitor),DecoratedVisitor) :-
	make_decorated_visitor([],Result,visitor:collect_path_to_vertices_pred,Visitor,DecoratedVisitor),!. 

collect_path_to_vertices_pred(V,P,continue,XS,[[P1,V]|XS]) :-
	reverse(P,P1).
		
expand_visitor_1(avoid_repetitions(Visitor),DecoratedVisitor) :-
	expand_visitor(Visitor,V1),	
	make_decorated_visitor([],_,visitor:avoid_rep_pred,V1,DecoratedVisitor),!.
expand_visitor_1(avoid_repetitions(InitialAvoidSet,Visitor),DecoratedVisitor) :-
	expand_visitor(Visitor,V1),	
	make_decorated_visitor(InitialAvoidSet,_,visitor:avoid_rep_pred,V1,DecoratedVisitor),!.	
expand_visitor_1(avoid_repetitions(InitialAvoidSet,FinalAvoidSet,Visitor),DecoratedVisitor) :-
	expand_visitor(Visitor,V1),	
	make_decorated_visitor(InitialAvoidSet,FinalAvoidSet,visitor:avoid_rep_pred,V1,DecoratedVisitor),!.	

avoid_rep_pred(V,_P,continue,XS,XS1) :-	
	\+ ord_memberchk(V,XS),!,
	ord_union([V],XS,XS1).
avoid_rep_pred(V,_P,stop,XS,XS) :-
	ord_memberchk(V,XS).

expand_visitor_1(depth_select(Depth,Visitor),DecoratedVisitor) :-	
	expand_visitor(Visitor,V1),	
	make_decorated_visitor([],_,visitor:depth_select_pred(Depth),V1,DecoratedVisitor),!.

depth_select_pred(Depth,_V,P,skip,XS,XS) :-	
	length(P,Depth1),	
	Depth1 < Depth,!.
depth_select_pred(Depth,_V,P,continue,XS,XS) :-
	length(P,Depth),!.
depth_select_pred(Depth,_V,P,stop,XS,XS) :-
	length(P,Depth1),
	Depth1 > Depth,!.
	
expand_visitor_1(avoid_cycles(Visitor),DecoratedVisitor) :-
	expand_visitor(Visitor,V1),	
	make_decorated_visitor([],_,visitor:avoid_cycles_pred,V1,DecoratedVisitor),!.

avoid_cycles_pred(V,P,continue,XS,XS) :-
	%format('avoid_cycles[1]: V=~k, P=~k',[V,P]), 
	\+ member([V,_],P),!.	
avoid_cycles_pred(V,P,stop,XS,XS) :-
	member([V,_],P),!.	
		