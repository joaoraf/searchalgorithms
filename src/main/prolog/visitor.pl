:- module(visitor,[
		make_visitor/2,
		make_visitor/3,
		make_visitor/4,
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

:- meta_predicate make_visitor(5,+,*,-).

make_visitor(VisitorPred,InitialState,FinalState,Visitor) :-	
	Visitor = visitor{		
		visitor_pred: VisitorPred,
		wrap: visitor:const_rel(InitialState),
		unwrap: visitor:id_rel2(FinalState)
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
	(Action1 = continue -> 	 	      
	    call(VisitorPred,Vertice,Path,Action,UnderInState,UnderOutState) ;
	    (Action = Action1, UnderOutState = UnderInState)
	).  

wrap_on_pair(OverState,UnderWrap,UnderState,ResultState) :-	
	call(UnderWrap,UnderState,WrappedState),
	ResultState = [OverState,WrappedState].
	

unwrap_from_pair(FinalState,UnderUnwrap,WrappedState,State) :-
	WrappedState = [OverState,UnderState],
	FinalState = OverState,	
	call(UnderUnwrap,UnderState,State).

unwrap_and_ignore(UnderUnwrap,UnderState,UnderState) :-
	call(UnderUnwrap,UnderState,_State).
	
const_rel(X,_Y,X).

id_rel(X,X).

id_rel2(X,X,_).

expand_visitor(VisitorExpr,Visitor) :-
	is_dict(VisitorExpr,visitor),
	Visitor = VisitorExpr,!.
expand_visitor(VisitorExpr,Visitor) :-
	\+ is_dict(VisitorExpr,visitor),	
	expand_visitor_1(VisitorExpr,VisitorExpr1),
	expand_visitor(VisitorExpr1,Visitor),!.
expand_visitor(VisitorExpr,_Visitor) :-
	format('Invalid expression: ~p\n',[VisitorExpr]),!, fail.	
	

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
	
expand_visitor_1(count(Count,Visitor),DecoratedVisitor) :-
	make_decorated_visitor(0,Count,visitor:count_visitor_pred,Visitor,DecoratedVisitor),!. 

count_visitor_pred(_V,_P,continue,Count,Count1) :-
	Count1 is Count + 1.

expand_visitor_1(trace(Visitor),DecoratedVisitor) :-
	make_decorated_visitor(0,_,visitor:trace_visitor_pred,Visitor,DecoratedVisitor),!. 

trace_visitor_pred(V,P,continue,Count,Count1) :-
	Count1 is Count + 1,
	reverse(P,P1),
	path_to_string(PathStr,P1),
	format('[~k] ~s ~k\n',[Count,PathStr,V]).		

path_to_string("",[]).	
path_to_string(Res,[[V,E]|R]) :- 
	format(string(S),' ~k --(~k)-->',[V,E]),
	path_to_string(Res1,R),
	string_concat(S,Res1,Res).	
	
expand_visitor_1(find_first(Pred,Value),DecoratedVisitor) :-
	make_visitor(visitor:find_first_visitor_pred(Pred),Value,DecoratedVisitor),!.
find_first_visitor_pred(Pred,V,P,continue,Value,Value) :-	
	var(Value),
	call(Pred,V,P),!,
	Value = [V,P].
find_first_visitor_pred(_Pred,_V,_P,stop,Value,Value) :-
	nonvar(Value), !.
find_first_visitor_pred(_Pred,_V,_P,continue,Value,Value) :- !.

expand_visitor_1(find_all(Pred,Value),DecoratedVisitor) :-
	make_visitor(visitor:find_all_visitor_pred(Pred),Value,DecoratedVisitor),!.
find_all_visitor_pred(Pred,V,P,continue,Value,NewValue) :-	
	var(Value),
	call(Pred,V,P),!,
	Value = [[V,P]|NewValue].
find_all_visitor_pred(_Pred,_V,_P,continue,Value,Value) :- !.
	
expand_visitor_1(find_at_most(AtMost,Pred,Value),DecoratedVisitor) :-
	make_visitor(visitor:find_at_most_visitor_pred(Pred),[Value,AtMost],DecoratedVisitor),!.
find_at_most_visitor_pred(Pred,V,P,continue,[Value,AtMost],[NewValue,AtMost1]) :-
	AtMost > 0,
	AtMost1 is AtMost-1,	
	var(Value),
	call(Pred,V,P),!,
	Value = [[V,P]|NewValue].
find_at_most_visitor_pred(_Pred,_V,_P,continue,State,State) :- !.

expand_visitor_1(find_best(ComparePred,Value),DecoratedVisitor) :-
	make_visitor(visitor:find_best_visitor_pred(ComparePred),not_found,Value,DecoratedVisitor),!.
find_best_visitor_pred(_Pred,V,P,continue,not_found,[V,P]) :- !.	
find_best_visitor_pred(ComparePred,V,P,continue,[V1,P1],[V,P]) :-
	call(ComparePred,(<),[V,P],[V1,P1]),!.
find_best_visitor_pred(_Pred,_V,_P,continue,State,State) :-  !.



expand_visitor_1(bound(BoundaryPred,CompareValuePred,Value),DecoratedVisitor) :-
	make_visitor(visitor:bound_visitor_pred(BoundaryPred,CompareValuePred),undefined,Value,DecoratedVisitor),!.
bound_visitor_pred(BoundaryPred,_CompareValuePred,V,P,continue,undefined,[V,P,Value]) :- 	
	call(BoundaryPred,[V,P],solution(Value)),	
	!.
bound_visitor_pred(BoundaryPred,_CompareValuePred,V,P,continue,undefined,undefined) :- 
	call(BoundaryPred,[V,P],lower_bound(_Value)),	
	!.			
bound_visitor_pred(BoundaryPred,CompareValuePred,V,P,continue,[_V1,_P1,Value1],[V,P,Value]) :-	
	call(BoundaryPred,[V,P],solution(Value)),	
	call(CompareValuePred,(<),Value,Value1),!.	
bound_visitor_pred(BoundaryPred,CompareValuePred,V,P,continue,State,State) :-	
	State = [_V1,_P1,Value1],	
	call(BoundaryPred,[V,P],lower_bound(Value)),	
	\+ call(CompareValuePred,(>),Value,Value1),!.	
bound_visitor_pred(_BoundaryPred,_CompareValuePred,_V,_P,stop,State,State) :- !.

expand_visitor_1(skip_if(Cond,Visitor),DecoratedVisitor) :-
	make_decorated_visitor(_,_,visitor:skip_if_visitor_pred(Cond),Visitor,DecoratedVisitor),!. 

skip_if_visitor_pred(Cond,V,P,skip,_,_) :-
	call(Cond,V,P),!.
skip_if_visitor_pred(_Cond,_V,_P,continue,_,_).

expand_visitor_1(look_for_node(Node,Visitor),DecoratedVisitor) :-
	make_decorated_visitor(_,_,visitor:look_for_node_pred(Node),Visitor,DecoratedVisitor),!. 

look_for_node_pred(V,V,_P,continue,S,S) :- !.
look_for_node_pred(_V1,_V2,_P,skip,S,S) :- !.
	
			
	