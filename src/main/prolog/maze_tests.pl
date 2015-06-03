:- module(maze_tests,[
		maze_tests/1,
		maze_test_options/1,
		maze_test_options/2,
		make_test_term/4,
		make_test_term/7,
		run_test/2
	]).
	
:- use_module(graph).
:- use_module(breadth_first_traversal).
:- use_module(depth_first_traversal).
:- use_module(iterative_deepening_traversal).
:- use_module(map).
:- use_module(visitor).
:- use_module(library(ansi_term)).

:- dynamic(maze_rel/2).

register_maze(Sym,Start,End,Maze) :-
	format('Registering maze: ~k\n',[Sym]),
	retractall(maze_rel(Sym,_)),
	edge_rel_from_map(4,Maze,MazeRel),
	MazeData = maze{
		start : Start,
		end : End,
		maze : Maze,
		rel : MazeRel
	},
	asserta(maze_rel(Sym,MazeData)).

maze_syms(Syms) :- aggregate(set(Sym),maze_rel(Sym,_),Syms).

:- initialization((forall(maze_data(Sym,Maze,Start,End),register_maze(Sym,Start,End,Maze)))). 	

maze_data(maze1,[
		[w,w,w,w,w,w,w,w,w,w],
		[w,f,f,f,f,f,f,f,f,w],
		[w,f,w,w,w,w,w,w,f,w],
		[w,f,f,f,f,f,f,f,f,w],
		[w,f,w,w,w,w,w,w,w,w],
		[w,f,f,f,f,f,f,f,f,w],
		[w,f,w,f,w,f,w,w,f,w],
		[w,f,w,f,w,f,w,w,f,w],
		[w,f,f,f,f,f,f,w,f,w],
		[w,w,w,w,w,w,w,w,w,w]
	],[1,1],[8,8]).
maze_data(maze2,[
		[f,f,f,f,f,f,f,f,f,f,f,f],
		[f,w,f,f,w,w,w,w,w,w,w,f],
		[f,w,f,f,w,f,f,f,f,f,w,f],
		[f,w,f,f,w,w,w,w,w,f,w,f],
		[f,f,f,f,f,f,f,f,w,f,w,f],
		[f,w,f,f,f,f,f,f,w,f,w,f],
		[f,w,w,w,w,w,w,f,w,f,w,f],
		[f,w,f,f,f,f,f,f,w,f,w,f],
		[f,w,f,w,w,w,w,f,w,f,w,f],
		[f,w,f,f,f,f,f,f,f,f,f,f]
	],[0,0],[5,2]).	
maze_data(maze3,[
		[w,w,w,w,w,w,w,w,w,w],
		[w,f,f,f,f,f,f,f,f,w],
		[w,f,w,w,w,w,w,w,f,w],
		[w,f,f,f,f,f,f,f,f,w],
		[w,f,w,w,w,w,w,w,w,w],
		[w,f,f,f,f,f,f,f,f,f],
		[w,f,w,f,w,f,w,w,w,f],
		[w,f,w,f,w,f,w,w,f,f],
		[w,f,f,f,f,f,f,w,f,w],
		[w,w,w,w,w,w,f,f,f,w]
	],[1,1],[8,8]).	

maze(Sym,P1,Move,P2) :-
	maze_rel(Sym,MazeData),
	call(MazeData.rel(P1,Move,P2)).	

maze_test_options(O1,O) :-	
  O = maze_test_options{
  	maze: MazeSym,
	traversal: Traversal, % dft, idt, bft
	order: Order, %keep, reorder
	search_type: SearchType, %exhaustive,branch_and_bound
	rounds: Rounds,
	time_limit_per_round: TimeLimit,
	trace_visit: Trace,
	queue_type: QueueType
  },
  (var(O1) -> O1 = O ; O >:< O1),   
  maze_rel(MazeSym,_),
  member(Traversal,[dft,idt,bft]),
  member(Order,[keep,reorder]),
  member(SearchType,[exhaustive,branch_and_bound]),
  (var(Rounds) *-> Rounds = 10 ; true),
  (var(TimeLimit) *-> TimeLimit = 6000 ; true),
  (var(Trace) *-> Trace = no ; true),   
  (Traversal = bft ->
  	member(QueueType,[normal,priority]) ; true)
  .

maze_test_options(O) :-	copy_term(O,O1), maze_test_options(O1,O).

:- dynamic maze_test_result/2.
  
init_results(Sym) :-  
  gensym(maze_test_result,Sym),
  retractall(maze_test_result(Sym,_)),
  empty_assoc(Successes),
  Results = results{test_count: 0, success_count: 0, failure_count: 0, successes: Successes, failures: []},
  asserta(maze_test_result(Sym,Results)).

register_success(Sym,O,TotalTime,Term,Len,Path) :-
  maze_test_result(Sym,ResultIn),
  TestCount is ResultIn.test_count + 1,
  SuccessCount is ResultIn.success_count + 1,
  SuccessesIn = ResultIn.successes,
  (get_assoc(TotalTime,SuccessesIn,TotalTimeBin),! ; TotalTimeBin = []),
  append(TotalTimeBin,[success_record{option: O, term: Term, length: Len, path: Path}],NewTotalTimeBin),
  put_assoc(TotalTime,SuccessesIn,NewTotalTimeBin,SuccessesOut),
  ResultOut = ResultIn.put(_{test_count: TestCount, success_count: SuccessCount, successes: SuccessesOut}),
  retractall(maze_test_result(Sym,_)),
  asserta(maze_test_result(Sym,ResultOut))
  .

register_failure(Sym,O,Term) :-  
  maze_test_result(Sym,ResultIn),
  TestCount is ResultIn.test_count + 1,
  FailureCount is ResultIn.failure_count + 1,
  append(ResultIn.failures,[[O,Term]],Failures), 
  ResultOut = ResultIn.put(_{test_count: TestCount, failure_count: FailureCount, failures: Failures}),
  retractall(maze_test_result(Sym,_)),
  asserta(maze_test_result(Sym,ResultOut)).

print_results(Sym) :-
	maze_test_result(Sym,Results),
	write('\n\n\n----------------------------------\n'),
	write('Results:\n'),
	format('  Total tests: ~k\n',[Results.test_count]),
	format('  Total successes: ~k\n',[Results.success_count]),
	format('  Total failures: ~k\n',[Results.failure_count]),
	((\+ empty_assoc(Results.successes)) ->
	   ( write('Successes:\n'),
	     min_assoc(Results.successes,TotalTime,Options),
	     format('   Best time: ~k ms\n',[TotalTime]),
	     write('   Best runs:\n'),	     
	     forall(member([O,Term],Options),format('     ~p\n       term: ~p\n',[O,Term])),
	     write('   All runs:\n'),
	     assoc_to_list(Results.successes,OptionsL),	         
	     forall(
	     	(member(Time-Options_1,OptionsL),
	     		member(Data,Options_1),
	     		Data = _{option:O, term: Term, length: Len, path: Path}),	     	
	     	format('     [~p] ~p ms: ~p\n       path:~p\n       term: ~p\n',
	     		[Len,Time,O,Path,Term]))
	   ) ; true),
	(Results.failures = [_|_] ->
	   ( write('Failures:\n'),	     	    
	     forall(member([O,Term],Results.failures),format('     ~p\n       term: ~p\n',[O,Term]))
	   ) ; true),
	retractall(maze_test_result(Sym,_)).
  
maze_tests(O) :- !,
	init_results(ResultSym),	
	forall(maze_test_options(O,O1),maze_test_run(ResultSym,O1)),
	print_results(ResultSym).

build_term(bft,QueueType,Visitor1,Rel1,Start,Term) :- 		
	Term = bft(Visitor1,Rel1,Start,QueueType,_,_),!.
build_term(Traversal,_QueueType,Visitor1,Rel1,Start,Term) :-	
	Traversal \= bft, 
	functor(Term,Traversal,5),
	arg(1,Term,Visitor1),
	arg(2,Term,Rel1),
	arg(3,Term,Start)
	.

make_test_term(O,Visitor,Term,Value) :- make_test_term(O,Visitor,Term,_Count,Value,_Rel1,_Start).
make_test_term(O,Visitor2,Term,Count,Value,Rel1,Start) :-
	maze_rel(O.maze,MazeData),
	Start = MazeData.start,
	Target = MazeData.end,
	Rel = MazeData.rel,
	UpperBoundSym = _,
	(O.traversal = bft -> (
	    O.queue_type = priority -> (
		gensym(maze_test_upper_bound,UpperBoundSym),
		QueueType = priority(map:total_distance(Target),UpperBoundSym) 
	    ) ;
	    QueueType = normal
	  ) ; true),
	format('traversal=~p, queue_type=~p, upperboundsym=~p\n',[O.traversal,QueueType,UpperBoundSym]),  
	(O.order = reorder -> Rel1 = map:reorder_by_distance(Target,Rel) ; Rel1 = Rel),
	(O.search_type = exhaustive -> T1 = look_for_node(Target,find_best(graph:compare_path_lengths,UpperBoundSym,Value)) ;
	                               T1 = bound(map:distance_bounds(Target),compare,Value)),	                              
	(O.traversal = idt -> Visitor0 = T1 ; 
	 O.traversal = bft -> Visitor0 = avoid_repetitions(T1) ;
	                      Visitor0 = avoid_cycles(T1)),
	Visitor1 = count(Count,Visitor0),
	(O.trace_visit = yes -> Visitor2 = trace_visit(Visitor1) ; Visitor1 = Visitor2),
	expand_visitor(Visitor2,ExpVisitor),
	build_term(O.traversal,QueueType,ExpVisitor,Rel1,Start,Term).
	
maze_test_run(ResultSym,O) :-
	make_test_term(O,Visitor,Term,Count,Value,Rel1,Start),		
	TimeLimit is O.time_limit_per_round * O.rounds / 1000.0,
	build_term(O.traversal,O,Visitor,Rel1,Start,Term1),
	copy_term(Term1,TermCopy),	
	catch(setup_call_catcher_cleanup(
		( print_test_header(O),
		  format('Running the test(time limit: ~k s): ',[TimeLimit]),ttyflush,
		  statistics(cputime,Time1) ),		
		  call_with_time_limit(TimeLimit,run_test(O.rounds,Term)),		  
		  Catcher,
		  ( statistics(cputime,Time2),
		    TotalTime is (Time2 - Time1) * 1000.0,		  		          
		    process_result(Catcher,O,Value,TotalTime,Count,ResultSym,TermCopy)
		  )),_,true).

run_test(1,Term) :-		
	call(Term),!,
	write('!\n'),ttyflush.
run_test(1,_Term) :-	
	write('?'),ttyflush,
	throw(test_failed).	
run_test(N,Term) :-
	N > 1,!,
	ignore((call(Term),!,fail)),!,
	format('~k.',N),ttyflush,
	N1 is N - 1,
	run_test(N1,Term).	

print_test_header(O) :-		  
	format('Maze Test\n-----------------------\n'),
	dump_options(O),
	format('------------------------\n').

process_result(_,O,not_found,_TotalTime,_Count,ResultSym,Term) :-
	register_failure(ResultSym,O,Term),
	ansi_format([bold,fg(red)],'\nstrategy failed: could not find a solution !\n',[]),!.			  
process_result(fail,O,_Value,_TotalTime,_Count,ResultSym,Term) :-
	register_failure(ResultSym,O,Term),
	ansi_format([bold,fg(red)],'\nstrategy failed!\n',[]),!.
process_result(exception(test_failed),O,_Value,_TotalTime,_Count,ResultSym,Term) :-
	register_failure(ResultSym,O,Term),
	ansi_format([bold,fg(red)],'\nstrategy failed!\n',[]).		
process_result(exception(Ex),O,_Value,_TotalTime,_Count,ResultSym,Term) :-
	register_failure(ResultSym,O,Term),
	ansi_format([bold,fg(red)],'\nstrategy raised an exception: ',[]),	
	print_message(error,Ex),!.
process_result(external_exception(Ex),O,_Value,_TotalTime,_Count,ResultSym,Term) :-
	register_failure(ResultSym,O,Term),
	ansi_format([bold,fg(red)],'\nexternal exception raised: ',[]),
	print_message(error,Ex),!.
process_result(_,O,Value,TotalTime,Count,ResultSym,Term) :- !,
	ansi_format([bold,fg(green)],'Strategy succeeded!\n',[]),
	MeanTime is TotalTime / O.rounds,	  							     	 		      
	Value = [_|[P|_]],
	reverse(P,P1),	
	((findall(M,member([_,M],P1),Moves),!);Moves=[]),	
	normalize_moves(Moves,Path),
	moves_cost(Path,Len),	
	register_success(ResultSym,O,MeanTime,Term,Len,Path),
	format('Path: ~k\n',[Path]),
	format('Total time: ~k ms\n',[TotalTime]),
	format('Mean time: ~k ms\n',[MeanTime]),
	format('Number of steps: ~k\n',[Count]).

dump_options(O) :-
	maze_test_options(O),
	format('Options:\n'),
	format('   maze: ~p,\n',[O.maze]),
	format('   traversal: ~p,\n',[O.traversal]),
	format('   order: ~p,\n',[O.order]),
	format('   search_type: ~p,\n',[O.search_type]),
	format('   rounds: ~p.\n',[O.rounds]),
	format('   time_limit_per_round: ~p\n',[O.time_limit_per_round]),
	format('   trace_visit: ~p\n',[O.trace_visit]),
	format('   queue_type: ~p\n',[O.queue_type]).		
	 
	 
	