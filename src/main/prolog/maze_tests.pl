:- module(maze_tests,[
		maze_tests/1,
		maze_test_options/1
	]).
	
:- use_module(graph).
:- use_module(breadth_first_traversal).
:- use_module(depth_first_traversal).
:- use_module(iterative_deepening_traversal).
:- use_module(map).
:- use_module(visitor).



:- dynamic(maze_rel/2).

register_maze(Sym,Start,End,Maze) :-
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

:- initialization((maze_data(Sym,Maze,Start,End),register_maze(Sym,Start,End,Maze))). 	

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

maze(Sym,P1,Move,P2) :-
	maze_rel(Sym,MazeData),
	call(MazeData.rel(P1,Move,P2)).	

maze_test_options(O) :-
  O = maze_test_options{
  	maze: MazeSym,
	traversal: Traversal, % dft, idt, bft
	order: Order, %keep, reorder
	search_type: SearchType, %exhaustive,branch_and_bound
	rounds: Rounds
  },
  maze_rel(MazeSym,_),
  member(Traversal,[dft,idt,bft]),
  member(Order,[keep,reorder]),
  member(SearchType,[exhaustive,branch_and_bound,a_star]),
  var(Rounds) *-> Rounds = 100 ; true.
  
maze_tests(O) :-
	maze_test_options(O),
	maze_test_run(O).
	
maze_test_run(O) :-
	maze_rel(O.maze,MazeData),
	Start = MazeData.start,
	Target = MazeData.end,
	Rel = MazeData.rel,
	(O.order = reorder -> Rel1 = map:reorder_by_distance(Target,Rel) ; Rel1 = Rel),
	(O.search_type = exhaustive -> T1 = find_best(map:distance_compare(Target),Value) ;
	                               T1 = bound(map:distance_bounds(Target),compare,Value)),	                              
	Visitor = count(Count,avoid_cycles(T1)),
	expand_visitor(Visitor,Visitor1),
	(O.traversal = dft -> Term = dft(Visitor1,Rel1,Start,_,_) ;
	 O.traversal = idt -> Term = idt(Visitor1,Rel1,Start,_,_) ;
	 		      Term = bft(Visitor1,Rel1,Start,_,_)),
	statistics(cputime,Time1),
	ignore((between(2,O.rounds,_),call(Term),write('.'),fail)),
	call(Term),
	statistics(cputime,Time2),
	MeanTime is (Time2 - Time1) / O.rounds,
	Value = [_|P],
	reverse(P,P1),
	findall(M,member([_,M],P1),Moves), 
	normalize_moves(Moves,Path),
	format('Maze Test\n-----------------------\n'),
	dump_options(O),
	format('------------------------\n'),
	format('Path: ~k\n',[Path]),
	format('Mean time: ~k\n',[MeanTime]),
	format('Number of steps: ~k\n',[Count]).

dump_options(O) :-
	maze_test_options(O),
	format('Options:\n'),
	format('   maze: ~p,\n',[O.maze]),
	format('   traversal: ~p,\n',[O.traversal]),
	format('   order: ~p,\n',[O.order]),
	format('   search_type: ~p,\n',[O.search_type]),
	format('   rounds: ~p.\n',[O.rounds]).	
	 
	 
	