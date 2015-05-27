:- module(depth_first_search,[ 	   
 	   dft//3
	]).

:- use_module(dcg_extras).

:- use_module(visitor).

:- dynamic dft_visit_check/2.

:- meta_predicate dft(+,3,+,+,-).
	
dft(Visitor,EdgeRel,Vertice) --> 
	{ gensym(dft_visit_check,VisitSym),
	  retractall(dft_visit_check(VisitSym,_)),
	  assertz(dft_visit_check(VisitSym,no)) },
	wrapping(Visitor,dft_0(EdgeRel,Vertice,VisitSym)),
	{ dft_visit_check(VisitSym,Check),
	  retractall(dft_visit_check(VisitSym,_)) },
	{ Check = yes -> true ; fail }.
				
dft_0(EdgeRel,Vertice,VisitSym,Visitor) --> dft_0(Visitor,EdgeRel,VisitSym,Vertice,[]).		

dft_0(Visitor,EdgeRel,VisitSym,Vertice,Path) -->
        call_visitor(Visitor,Vertice,Path,Action),         	
	dft_1(Visitor,EdgeRel,VisitSym,Vertice,Path,Action). 	

dft_1(_Visitor,_EdgeRel,_VisitSym,_Vertice,_Path,stop) --> { ! },
	dcg_true.
dft_1(Visitor,EdgeRel,VisitSym,Vertice,Path,skip) -->	{ ! },
	dcg_foreach(call(EdgeRel,Vertice,Edge,Vertice1),	 
	            dcg_optional(dft_0(Visitor,EdgeRel,VisitSym,Vertice1,[[Vertice,Edge]|Path]))).	
dft_1(Visitor,EdgeRel,VisitSym,Vertice,Path,continue) --> { ! } , 	
	 {(dft_visit_check(VisitSym,no) -> (
	  	retractall(dft_visit_check(VisitSym,_)),
	  	assertz(dft_visit_check(VisitSym,yes))
	   ) ; true) },	  
	dcg_optional(dcg_foreach(
		call(EdgeRel,Vertice,Edge,Vertice1),	 
	        dcg_optional(dft_0(Visitor,EdgeRel,VisitSym,Vertice1,[[Vertice,Edge]|Path])))).
dft_1(_Visitor,_EdgeRel,VisitSym,Vertice,Path,Action) --> { ! },
	{ format('dft_1: error: Action = ~p, VisitSym=~p, Vertice=~p, Path=~p\n',[Action,VisitSym,Vertice,Path]), trace }.  	            	