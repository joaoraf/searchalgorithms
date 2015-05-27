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
	idt(Visitor,EdgeRel,Vertice,0).

idt(Visitor,EdgeRel,Vertice,Depth) -->  { ! },    
  {    
    Depth1 is Depth + 1,
    expand_visitor(depth_select(Depth,Visitor),DecoratedVisitor)       
  },    
  dcg_if(
  	dft(DecoratedVisitor,EdgeRel,Vertice),
  	dcg_optional(idt(Visitor,EdgeRel,Vertice,Depth1)),        
        dcg_true
  ). 

