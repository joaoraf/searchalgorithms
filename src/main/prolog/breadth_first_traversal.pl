:- module(breadth_first_search,[ 	   
 	   bft//3
	]).

:- use_module(dcg_extras).

:- use_module(visitor).

:- meta_predicate bft(+,3,+,+,-).
	
bft(Visitor,EdgeRel,Vertice) --> wrapping(Visitor,bft_0(EdgeRel,Vertice)).
				
bft_0(EdgeRel,Vertice,Visitor) --> bft_0(Visitor,EdgeRel,[[[[],Vertice]]],[]).		

bft_0(_Visitor,_EdgeRel,[],[]) --> { ! },dcg_true.
bft_0(Visitor,EdgeRel,[[]|XS],YS) --> { ! },
	bft_0(Visitor,EdgeRel,XS,YS).
bft_0(Visitor,EdgeRel,XS,[[]|YS]) --> { ! },
	bft_0(Visitor,EdgeRel,XS,YS).	
bft_0(Visitor,EdgeRel,[],RevQueue) --> 
	{ reverse(RevQueue,Queue) },
	bft_0(Visitor,EdgeRel,Queue,[]).
bft_0(Visitor,EdgeRel,[[[Path,Vertice]|Q]|Queue],RevQueue) -->
        call_visitor(Visitor,Vertice,Path,Action),         	
	bft_1(Visitor,EdgeRel,Vertice,Path,Action,[Q|Queue],RevQueue). 	

bft_1(Visitor,EdgeRel,_Vertice,_Path,stop,Queue,RevQueue) --> 
	bft_0(Visitor,EdgeRel,Queue,RevQueue).	
bft_1(Visitor,EdgeRel,Vertice,Path,continue,Queue,RevQueue) -->
	{ (
		findall(
			[[[Vertice,Edge]|Path],Vertice1],
			call(EdgeRel,Vertice,Edge,Vertice1),
			NextVisits),
	  	RevQueue1 = [NextVisits | RevQueue],
	  	! ) ;
	  (  RevQueue1 = RevQueue ) },
	bft_0(Visitor,EdgeRel,Queue,RevQueue1).	 
	        