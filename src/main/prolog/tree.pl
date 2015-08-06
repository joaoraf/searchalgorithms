:- module(tree,[
	value/2,
	subtree/3,
	dump_tree/1,
	visit/1,
	visitN//1 
	]).

value(tree(X,_),X).

subtree(tree(_,L),E,T) :- member([E,T],L).

dump_tree(tree(N,L)) :-  
  nonvar(L),!,
  (L = [] ; L = [_|_]),
  format('~k\n',[N]),   
  forall(member([E,T],L),dump_tree(E,T,'|')).
dump_tree(tree(N,L)) :-  
  var(L),!,
  format('~k\n| ...\n',[N]).   

dump_tree(E,tree(N,L),S) :- 
  nonvar(L),!,    
  (L = [] ; L = [_|_]),
  format(atom(P),'--[~k]--> ',[E]),
  atom_length(P,LP),
  length(S1,LP),
  maplist(=(' '),S1),
  atomic_list_concat(S1,S2),
  atomic_list_concat([S,S2],S3),
  format(atom(S4),'~s|',[S3]),
  format('~s~s~k\n',[S,P,N]),  
  forall(member([E1,T1],L),dump_tree(E1,T1,S4)).
  
dump_tree(E,tree(N,L),S) :-
  var(L),!,  
  format(atom(P),'|--[~k]-- ',[E]),
  atom_length(P,LP),
  length(S1,LP),
  maplist(=(' '),S1),
  atomic_list_concat(S1,S2),
  format(atom(S3),'~s|',[S2]),
  format('~s~s~k\n',[S,P,N]),  
  format('~s ...\n',[S3]).      
        

visitall([[_,X]|XS]) :-   
  visit(X),  
  visitall(XS),!.
visitall([]) :- !.  		
  		
visit(tree(_N,XS)) :-  
  (XS = [_|_] ; XS = []),!,         
  visitall(XS).

visitNall([[_,X]|XS]) -->   
  visitN(X),  
  visitNall(XS),!.
visitNall([]) --> !.

visitN(N,T) :- visitN(T,N,_).

visitN(_T,0,0) :- !.
visitN(tree(_N,XS)) -->
  {(XS = [_|_] ; XS = []),!},         
  visitNall(XS).  	