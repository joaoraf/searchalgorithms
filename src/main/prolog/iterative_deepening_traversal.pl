:- module(iterative_deepening_traversal,[
	idt//3,
	idt//4
	]).

:- use_module(dcg_extras).
:- use_module(visitor).
:- use_module(depth_first_traversal).

:- meta_predicate idt(+,3,+,+,-).
:- meta_predicate idt(+,3,+,+,+,-).

idt(Visitor,EdgeRel,Vertice) -->
	idt(Visitor,EdgeRel,Vertice,yes).
	
idt(Visitor,EdgeRel,Vertice,Cyclic) -->   
   wrapping(Visitor,idt_0(EdgeRel,Vertice,Cyclic)).

idt_0(EdgeRel,Vertice,Cyclic,Visitor) -->
	idt(Visitor,EdgeRel,Vertice,0,Cyclic).

idt(Visitor,EdgeRel,Vertice,Depth,Cyclic) -->  { ! },    
  {     
    Depth1 is Depth + 1,
    (Cyclic = yes -> Visitor1=avoid_cycles(depth_select(Depth,Visitor,AnyVisit)) ;
                     Visitor1=depth_select(Depth,Visitor,AnyVisit))
  },    
  dft(Visitor1,EdgeRel,Vertice),
  idt_cont(AnyVisit,Visitor,EdgeRel,Vertice,Depth1,Cyclic).

idt_cont(yes,Visitor,EdgeRel,Vertice,Depth,Cyclic) -->
  idt(Visitor,EdgeRel,Vertice,Depth,Cyclic).
idt_cont(no,_Visitor,_EdgeRel,_Vertice,_Depth,_Cyclic) --> { true }.
