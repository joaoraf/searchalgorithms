:- module(dcg_extras,[
	dcg_foreach//2,
	dcg_true//0,
	dcg_if//3,
	dcg//1
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

:- meta_predicate dcg(0,+,-).

dcg(P,In,In) :-
	call(P).	