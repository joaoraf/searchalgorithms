:- module(depth_first_search,[ 	   
 	   dft//3
	]).

:- use_module(dcg_extras).

:- use_module(visitor).

:- meta_predicate dft(+,3,+,+,-).
	
dft(Visitor,EdgeRel,Vertice) --> wrapping(Visitor,dft_0(EdgeRel,Vertice)).
				
dft_0(EdgeRel,Vertice,Visitor) --> dft_0(Visitor,EdgeRel,Vertice,[]).		

dft_0(Visitor,EdgeRel,Vertice,Path) -->
        call_visitor(Visitor,Vertice,Path,Action),         	
	dft_1(Visitor,EdgeRel,Vertice,Path,Action). 	

dft_1(_Visitor,_EdgeRel,_Vertice,_Path,stop) --> 
	dcg_true.
dft_1(Visitor,EdgeRel,Vertice,Path,continue) -->
	dcg_foreach(call(EdgeRel,Vertice,Edge,Vertice1),	 
	            dft_0(Visitor,EdgeRel,Vertice1,[[Vertice,Edge]|Path])).	