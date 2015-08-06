:- module(lazy_tree_examples,[
	divisor_tree/2,
	lazy_divisor_tree/2
   ]).
   
:- use_module(lazy_tree).

:- use_module(lazy_list).

:- use_module(tree).

:- use_module(library(clpfd)).

divisor_cons(Num,Div) :- 	 
	Div #=< (Num div 2),
	Div #> 1, 
	Num mod Div #= 0.

divisor(Num,Div) :- 
	divisor_cons(Num,Div), 
	label([Num,Div]).

divisor_tree_subnode(Num,X,Y) :-
	divisor(Num,Y),
	X is Num div Y.

divisor_tree(Num,Tree) :-
	lazytree(Num,divisor_tree_subnode,Tree).

lazy_divisor_tree_next([E,V],[E,V,E1,V1],ll_cont([E,V])) :-	
	copy_term(E,E1),
	copy_term(V,V1),
	format('[1]Labelling: ~k,~k\n',[E,V]),
	label([E,V]),!,
	format('[1]Labelled: ~k,~k\n',[E,V]).	 
lazy_divisor_tree_next([LE,LV,E,V],[E,V,E1,V1],ll_cont([E,V])) :-
	format('[2]Start: LE=~k,LV=~k\n',[LE,LV]),
        E #\= LE, V #\= LV,
	copy_term(E,E1),
	copy_term(V,V1),
	format('[2]Labelling: ~k,~k\n',[E,V]),
	label([E,V]),!,
	format('[2]Labelled: ~k,~k\n',[E,V]).
lazy_divisor_tree_next([E,V],_,ll_end) :- 
	!, 
	format('[3]: no solutions E=~k, V=~k.\n',[E,V]).		 
lazy_divisor_tree_next([LE,LV,E,V],_,ll_end) :- 
	!, 
	format('[4]: no solutions LE=~k, LV=~k, E=~k, V=~k.\n',[LE,LV,E,V]).	
		
lazy_divisor_tree_subnode(Num,L) :-
	divisor_cons(Num,Y),
	X #> 1,
	X #< Num,
	X #= Num div Y,
	lazylist(lazy_divisor_tree_next,[X,Y],L).
	
		
lazy_divisor_tree(Num,Tree) :-
	lazytree1(Num,lazy_divisor_tree_subnode,Tree).