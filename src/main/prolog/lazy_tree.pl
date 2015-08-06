:- module(lazy_tree,[
		lazytree/3,
		lazytree1/3	
	]).

:- use_module(lazy_list).

:- meta_predicate lazytree(+,3,?).
:- meta_predicate lazytree1(+,3,?).


make_subtrees(RootValue,SubNodePred,SubTrees) :-	
	aggregate( 
         bag([EdgeValue,SubTree]),
         SubTreeValue^(
            call(SubNodePred,RootValue,EdgeValue,SubTreeValue),            
            lazytree(SubTreeValue,SubNodePred,SubTree)
         ),
         SubTrees
       ),!.
make_subtrees(_RootValue,_SubNodePred,[]).
       
lazytree(RootValue,SubNodePred,Tree) :-  
  Tree = tree(RootValue,SubTrees),  
  when(nonvar(SubTrees),make_subtrees(RootValue,SubNodePred,SubTrees)).
  

make_subtrees1(RootValue,SubNodesPred,EdgeSubTrees) :-
	call(SubNodesPred,RootValue,EdgeSubTreeValues),
	lazylist(make_subtrees_1_a(SubNodesPred),EdgeSubTreeValues,EdgeSubTrees).

make_subtrees_1_a(_SubNodesPred,PrevState,NextState,Result) :-
	PrevState = [],
	NextState = PrevState,
	Result = ll_end.
make_subtrees_1_a(SubNodesPred,PrevState,NextState,Result) :-
	PrevState = [[E,V]|XS],
	NextState = XS,
	lazytree1(V,SubNodesPred,Tree),
	Result = ll_cont([E,Tree]).
  
lazytree1(RootValue,SubNodesPred,Tree) :-
  Tree = tree(RootValue,SubTrees),  
  when(nonvar(SubTrees),make_subtrees1(RootValue,SubNodesPred,SubTrees)).