:- module(breadth_first_search,[ 	   
 	   bft//3,
 	   bft//4
	]).

:- use_module(dcg_extras).
:- use_module(visitor).
:- use_module(queues).

:- meta_predicate bft(+,3,+,+,-).

:- meta_predicate bft(+,3,+,+,+,-).
	
bft(Visitor,EdgeRel,Vertice) --> bft(Visitor,EdgeRel,Vertice,normal).	
	
bft(Visitor,EdgeRel,Vertice,QueueType) --> wrapping(Visitor,bft_0(EdgeRel,Vertice,QueueType)).

				
bft_0(EdgeRel,Vertice,QueueType,Visitor) -->
  { make_queue(QueueType,Queue),
    queue_put([[[],Vertice]],Queue,QueueIn) }, 
  bft_1(Visitor,EdgeRel,QueueIn,_QueueOut).		

bft_1(_Visitor,_EdgeRel,Qin,Qin) --> { queue_empty(Qin), ! },dcg_true.
bft_1(Visitor,EdgeRel,Qin,Qout) -->
        { queue_next([Path,Vertice],Qin,Qout1) },	
        call_visitor(Visitor,Vertice,Path,Action),                 
	bft_2(Visitor,EdgeRel,Vertice,Path,Action,Qout1,Qout). 	

bft_2(Visitor,EdgeRel,_Vertice,_Path,stop,Qin,Qout) --> 
	bft_1(Visitor,EdgeRel,Qin,Qout).		
bft_2(Visitor,EdgeRel,Vertice,Path,skip,Qin,Qout) -->
	bft_2(Visitor,EdgeRel,Vertice,Path,continue,Qin,Qout).	
bft_2(Visitor,EdgeRel,Vertice,Path,continue,Qin,Qout) -->
	{ (
		findall(
			[[[Vertice,Edge]|Path],Vertice1],
			call(EdgeRel,Vertice,Edge,Vertice1),
			NextVisits),
	  	queue_put(NextVisits,Qin,Qout1),
	  	! ) ;
	  (  Qout1 = Qin ) },
	bft_1(Visitor,EdgeRel,Qout1,Qout).	 

