:- module(iterative_deepening_traversal,[
	idt//3
	]).

:- use_module(dcg_extras).
:- use_module(visitor).
:- use_module(depth_first_traversal).

:- meta_predicate idt(+,3,+,+,-).

idt(Visitor,EdgeRel,Vertice) -->   
   wrapping(Visitor,idt_0(EdgeRel,Vertice)).

idt_0(EdgeRel,Vertice,Visitor) -->
	idt(Visitor,EdgeRel,Vertice,0,[]).

idt(Visitor,EdgeRel,Vertice,Depth,Visited1) -->    
  { 
    Depth1 is Depth + 1,
    expand_visitor(avoid_cycles(depth_select(Depth,avoid_repetitions(Visited1,Visited2,Visitor))),DecoratedVisitor),
    length(Visited1,N1)
  },    
  dcg_if(
  	dft(DecoratedVisitor,EdgeRel,Vertice),
  	dcg_if(
  		check_len(N1,Visited2),  	 
        	idt(Visitor,EdgeRel,Vertice,Depth1,Visited2),
        	dcg_true
        ),
        dcg_true
  ). 

check_len(N1,Visited2,State,State) :-
   length(Visited2,N2), 
   N1 < N2.

