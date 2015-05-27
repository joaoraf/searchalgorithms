:- module(dcg_extras,[
	dcg_foreach//2,
	dcg_true//0,
	dcg_if//3,
	dcg_or//2,
	dcg_optional//1,
	dcg//1,
	print_state//1	
	]).
	
:- meta_predicate dcg_foreach(0,2,+,-).
:- dynamic dcg_foreach_state/2.	

dcg_foreach(T,F,In,Out) :-	
        gensym(dcg_foreach_state,StateId),        
	setup_call_cleanup(
		asserta(dcg_foreach_state(StateId,In)),
		( forall(T,dcg_foreach_1(StateId,F))
		  ,dcg_foreach_state(StateId,Out)),
		retractall(dcg_foreach_state(StateId,_))).
dcg_foreach_1(StateId,F) :-
	dcg_foreach_state(StateId,In),
	call(F,In,Out),
	retractall(dcg_foreach_state(StateId,_)),
	assertz(dcg_foreach_state(StateId,Out)).
	
dcg_true(X,X).

:- meta_predicate dcg_if(2,2,2,+,-).

dcg_if(T,Then,_Else,In,Out) :-
	call(T,In,S1),!,
	call(Then,S1,Out).
	
dcg_if(_T,_Then,Else,In,Out) :-	
	call(Else,In,Out),!.	

:- meta_predicate dcg_or(2,2,+,-).

dcg_or(T1,_T2) --> T1, {!}.
dcg_or(_T1,T2) --> T2.	

:- meta_predicate dcg_optional(2,+,-).

dcg_optional(T) --> T, {!}.
dcg_optional(_T) --> dcg_true.

:- meta_predicate dcg(0,+,-).

dcg(P,In,In) :-
	call(P).
	
print_state(Msg,In,In) :-
	format('~s: ~p\n',[Msg,In]).		