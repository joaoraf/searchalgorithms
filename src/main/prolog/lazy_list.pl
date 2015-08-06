:- module(lazy_list,[	
	lazylist/4,
	lazylist/3
	]).

:- use_module(list_extra).

:- meta_predicate lazylist(3,+,?,?).
:- meta_predicate lazylist(3,+,?).

make_list(Pred,PrevState,LastState,List) :-
  call(Pred,PrevState,NextState,Result),
  format('   make_list: PrevState=~k, NextState=~k, Result=~k, List=~k\n',[PrevState,NextState,Result,List]),
  make_list(Pred,NextState,LastState,Result,List).

make_list(_Pred,NextState,LastState,ll_end,[]) :-
	format('make_list/5[1]: start\n'),
	LastState = NextState.
make_list(Pred,NextState,LastState,ll_cont(V),[V|VS]) :-
	format('make_list/5[2]: start\n'),
	lazylist(Pred,NextState,LastState,VS).

lazylist(Pred,State,List) :-
  lazylist(Pred,State,_,List).
  
lazylist(Pred,State,LastState,List) :-
  when(nonvar(List),
       make_list(Pred,State,LastState,List)).

mult_set([],_Factor,[]) :- !.
mult_set(Nums,Factor,NewNums) :-
  Nums = [_|_],!,
  aggregate(set(N),M^(member(M,Nums), N is Factor * M), NewNums).

inc_pred(N,N1,ll_cont(N)) :- N1 is N + 1.

inc_list(From,List) :- lazylist(inc_pred,From,_,List).

next_prime([X],Y,[]) :- Y is X + 1.
next_prime([X,Y|XS],Z,[Y|XS]) :- X + 1 < Y, Z is X + 1.
next_prime([X,Y|XS],Z,YS) :- 
  Y is X + 1,    
  next_prime([Y|XS],Z,YS).  
  
prime_numbers_list_pred(Avoid,NextAvoid,ll_cont(Prime)) :-	
	((Avoid = [_|_], next_prime(Avoid,Prime,Avoid1) ) ; 
	 (Avoid = [], Prime = 1, Avoid1 = [])),
	Prime1 is Prime + 1,
	range(1,Prime1,PrevNums),	
	mult_set(PrevNums,Prime,Avoid2),
	union(Avoid1,Avoid2,NextAvoid1),
	sort(NextAvoid1,NextAvoid2),
	NextAvoid = NextAvoid2.
		
prime_numbers(L) :- lazylist(prime_numbers_list_pred,[],_,L).
