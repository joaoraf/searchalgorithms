:- module(queues,
	[
		make_queue/2,
		queue_put/3,
		queue_next/3,
		queue_empty/1,
		set_upper_bound/2
	]).
				
make_queue(normal,[normal|Q]) :- normal_make_queue(Q).
make_queue(priority(KeyPred),[priority|Q]) :- priority_make_queue(KeyPred,_,Q).
make_queue(priority(KeyPred,UpperBoundSym),[priority|Q]) :- priority_make_queue(KeyPred,UpperBoundSym,Q).

queue_put(Xs,[normal|Q],[normal|Q1]) :- normal_queue_put(Xs,Q,Q1). 
queue_put(Xs,[priority|Q],[priority|Q1]) :- priority_queue_put(Xs,Q,Q1).

queue_next(X,[normal|Q],[normal|Q1]) :- normal_queue_next(X,Q,Q1). 
queue_next(X,[priority|Q],[priority|Q1]) :- priority_queue_next(X,Q,Q1).

queue_empty([normal|Q]) :- normal_queue_empty(Q). 
queue_empty([priority|Q]) :- priority_queue_empty(Q).

normal_make_queue([[],[],[]]).

normal_queue_put(Xs,[A,B,C],[A,B,[Xs|C]]).

normal_queue_next(A,[ [A|AS], BS    , CS], [AS , BS , CS ]).
normal_queue_next(X,[ []    , [AS|BS], CS], [AS1, BS1, CS1]) :- normal_queue_next(X,[AS,BS,CS],[AS1,BS1,CS1]).
normal_queue_next(X,[ []    , []    , CS], [AS1, BS1, [] ]) :- CS=[_|_], reverse(CS,BS), normal_queue_next(X,[[],BS,[]],[AS1,BS1,_]).

normal_queue_empty([[],[],[]]).


priority_make_queue(KeyPred,UpperBoundSym,[KeyPred,UpperBoundSym,A]) :- empty_assoc(A).

:- dynamic priority_upper_bound/2.

set_upper_bound(Sym,UpperBound) :-	
	nonvar(Sym),!,	
	retractall(priority_upper_bound(Sym,_)),
	asserta(priority_upper_bound(Sym,UpperBound)).
set_upper_bound(_Sym,_UpperBound) :- !.

has_upper_bound(UpperBoundSym,KeyPred,UpperBound) :-	
	nonvar(UpperBoundSym),
	priority_upper_bound(UpperBoundSym,UpperBound1),	
	call(KeyPred,UpperBound1,UpperBound).

priority_prod_next(KeyPred,UpperBoundSym,Xs,K-V) :-
	member(V,Xs),	
	call(KeyPred,V,K),	
	(has_upper_bound(UpperBoundSym,KeyPred,UpperBound) -> (		
		K < UpperBound 
	); true).

priority_queue_put(Xs,[KeyPred,UpperBoundSym,Q],[KeyPred,UpperBoundSym,Q1]) :-
  (findall(X,priority_prod_next(KeyPred,UpperBoundSym,Xs,X),KeyValuePairs),! ; 
    KeyValuePairs=[]),  
  group_pairs_by_key(KeyValuePairs, KeyValuesPairs),
  merge_all(Q,KeyValuesPairs,Q1).

merge_all(Q,[],Q).
merge_all(Q,[Key-Values|KVS],QF) :-
  ((get_assoc(Key,Q,Queue),!) ; normal_make_queue(Queue)),
  normal_queue_put(Values,Queue,Queue1),
  put_assoc(Key,Q,Queue1,Q1),
  merge_all(Q1,KVS,QF).


priority_queue_next(X,[KP,UpperBoundSym,Q],[KP,UpperBoundSym,Q2]) :-
  \+ empty_assoc(Q),
  del_min_assoc(Q, Key, Queue, Q1),  
  (has_upper_bound(UpperBoundSym,KP,UpperBound) -> (      
      Key >= UpperBound -> (      
          !,
          fail          
        ) ; true
    ) ; true),  
  normal_queue_next(X,Queue,Queue1),
  (normal_queue_empty(Queue1) -> Q2 = Q1 ; 
                                 put_assoc(Key,Q,Queue1,Q2)).
  
priority_queue_empty([_,_,Q]) :- empty_assoc(Q).
priority_queue_empty([KP,UpperBoundSym,Q]) :-
  min_assoc(Q, Key, _),
  has_upper_bound(UpperBoundSym,KP,UpperBound),
  Key >= UpperBound
  .
   	        	