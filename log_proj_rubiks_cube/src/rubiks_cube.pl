/*
	Rubiks cube solver - project at BUT FIT
	Author: Sebastian Krajnak
	Date: 24.4.2022
*/

% loading from input file copied from example input2.pl located at wis.fit.vutbr.cz/FIT/st/cfs.php.cs?file=%2Fcourse%2FFLP-IT%2Fprojects%2Flog%2Finput2.pl&cid=14578
/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

% -------------------------------------------------------------------------
% Map loaded cube structure into internal cube structure, easier to read, less room for error
map_cube([
	[[U1,U2,U3]],
	[[U4,U5,U6]],
	[[U7,U8,U9]],

	[[F1,F2,F3], [R1,R2,R3], [B1,B2,B3], [L1,L2,L3]],
	[[F4,F5,F6], [R4,R5,R6], [B4,B5,B6], [L4,L5,L6]],
	[[F7,F8,F9], [R7,R8,R9], [B7,B8,B9], [L7,L8,L9]],

	[[D1,D2,D3]],
	[[D4,D5,D6]],
	[[D7,D8,D9]]
	]
	,[
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	]).

% Print cube on stdout
print_cube([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,
	
	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9]
	) :-
	format("~w~w~w~n", [U1,U2,U3]),
	format("~w~w~w~n", [U4,U5,U6]),
	format("~w~w~w~n", [U7,U8,U9]),
	format("~w~w~w ~w~w~w ~w~w~w ~w~w~w~n", [F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3]),
	format("~w~w~w ~w~w~w ~w~w~w ~w~w~w~n", [F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6]),
	format("~w~w~w ~w~w~w ~w~w~w ~w~w~w~n", [F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9]),
	format("~w~w~w~n", [D1,D2,D3]),
	format("~w~w~w~n", [D4,D5,D6]),
	format("~w~w~w~n", [D7,D8,D9]).

%Checks if a cube is in a solved state
check_solved([
	U,U,U,
	U,U,U,
	U,U,U,

	F,F,F, R,R,R, B,B,B, L,L,L,
	F,F,F, R,R,R, B,B,B, L,L,L,
	F,F,F, R,R,R, B,B,B, L,L,L,
	
	D,D,D,
	D,D,D,
	D,D,D]).

% -------------------------------------------------------------------------
% Define possible CLOCKWISE cube rotations
rotate_up([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U7,U4,U1,
		U8,U5,U2,
		U9,U6,U3,
	
		R1,R2,R3, B1,B2,B3, L1,L2,L3, F1,F2,F3,
		F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
		F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,
	
		D1,D2,D3,
		D4,D5,D6,
		D7,D8,D9
		]).

rotate_front([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,U3,
		U4,U5,U6,
		L9,L6,L3,
	
		F7,F4,F1, U7,R2,R3, B1,B2,B3, L1,L2,D1,
		F8,F5,F2, U8,R5,R6, B4,B5,B6, L4,L5,D2,
		F9,F6,F3, U9,R8,R9, B7,B8,B9, L7,L8,D3,

		R7,R4,R1,
		D4,D5,D6,
		D7,D8,D9
		]).

rotate_right([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,F3,
		U4,U5,F6,
		U7,U8,F9,
	
		F1,F2,D3, R7,R4,R1, U9,B2,B3, L1,L2,L3,
		F4,F5,D6, R8,R5,R2, U6,B5,B6, L4,L5,L6,
		F7,F8,D9, R9,R6,R3, U3,B8,B9, L7,L8,L9,
	
		D1,D2,B7,
		D4,D5,B4,
		D7,D8,B1
		]).

rotate_back([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		R3,R6,R9,
		U4,U5,U6,
		U7,U8,U9,
	
		F1,F2,F3, R1,R2,D9, B7,B4,B1, U3,L2,L3,
		F4,F5,F6, R4,R5,D8, B8,B5,B2, U2,L5,L6,
		F7,F8,F9, R7,R8,D7, B9,B6,B3, U1,L8,L9,
	
		D1,D2,D3,
		D4,D5,D6,
		L1,L4,L7
		]).

rotate_left([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		B9,U2,U3,
		B6,U5,U6,
		B3,U8,U9,
	
		U1,F2,F3, R1,R2,R3, B1,B2,D7, L7,L4,L1,
		U4,F5,F6, R4,R5,R6, B4,B5,D4, L8,L5,L2,
		U7,F8,F9, R7,R8,R9, B7,B8,D1, L9,L6,L3,
	
		F1,D2,D3,
		F4,D5,D6,
		F7,D8,D9
		]).

rotate_down([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,U3,
		U4,U5,U6,
		U7,U8,U9,
	
		F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
		F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
		L7,L8,L9, F7,F8,F9, R7,R8,R9, B7,B8,B9,
	
		D7,D4,D1,
		D8,D5,D2,
		D9,D6,D3
		]).

% -------------------------------------------------------------------------
% Define possible COUNTER-CLOCKWISE cube rotations
rotate_cup([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U3,U6,U9,
		U2,U5,U8,
		U1,U4,U7,
	
		L1,L2,L3, F1,F2,F3, R1,R2,R3, B1,B2,B3,
		F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
		F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,
	
		D1,D2,D3,
		D4,D5,D6,
		D7,D8,D9
		]).

rotate_cfront([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,U3,
		U4,U5,U6,
		R1,R4,R7,
	
		F3,F6,F9, D3,R2,R3, B1,B2,B3, L1,L2,U9,
		F2,F5,F8, D2,R5,R6, B4,B5,B6, L4,L5,U8,
		F1,F4,F7, D1,R8,R9, B7,B8,B9, L7,L8,U7,
	
		L3,L6,L9,
		D4,D5,D6,
		D7,D8,D9
		]).

rotate_cright([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,B7,
		U4,U5,B4,
		U7,U8,B1,
	
		F1,F2,U3, R3,R6,R9, D9,B2,B3, L1,L2,L3,
		F4,F5,U6, R2,R5,R8, D6,B5,B6, L4,L5,L6,
		F7,F8,U9, R1,R4,R7, D3,B8,B9, L7,L8,L9,
	
		D1,D2,F3,
		D4,D5,F6,
		D7,D8,F9
		]).

rotate_cback([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		L7,L4,L1,
		U4,U5,U6,
		U7,U8,U9,

		F1,F2,F3, R1,R2,U1, B3,B6,B9, D7,L2,L3,
		F4,F5,F6, R4,R5,U2, B2,B5,B8, D8,L5,L6,
		F7,F8,F9, R7,R8,U3, B1,B4,B7, D9,L8,L9,

		D1,D2,D3,
		D4,D5,D6,
		R9,R6,R3
		]).

rotate_cleft([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		F1,U2,U3,
		F4,U5,U6,
		F7,U8,U9,
	
		D1,F2,F3, R1,R2,R3, B1,B2,U7, L3,L6,L9,
		D4,F5,F6, R4,R5,R6, B4,B5,U4, L2,L5,L8,
		D7,F8,F9, R7,R8,R9, B7,B8,U1, L1,L4,L7,
	
		B9,D2,D3,
		B6,D5,D6,
		B3,D8,D9
		]).

rotate_cdown([
	U1,U2,U3,
	U4,U5,U6,
	U7,U8,U9,

	F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
	F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
	F7,F8,F9, R7,R8,R9, B7,B8,B9, L7,L8,L9,

	D1,D2,D3,
	D4,D5,D6,
	D7,D8,D9
	],[
		U1,U2,U3,
		U4,U5,U6,
		U7,U8,U9,
	
		F1,F2,F3, R1,R2,R3, B1,B2,B3, L1,L2,L3,
		F4,F5,F6, R4,R5,R6, B4,B5,B6, L4,L5,L6,
		R7,R8,R9, B7,B8,B9, L7,L8,L9, F7,F8,F9,
	
		D3,D6,D9,
		D2,D5,D8,
		D1,D4,D7
		]).

% -------------------------------------------------------------------------
% State search and cube solve
/*
pokud vezmeš ten move, tak můžeš implementovat buď tak, že vypočteš další stavy z aktuálních a ty 
appendneš do listu a nad tím spustíš další solve (BFS) a nebo uděláš move, který ti vytvoří nový stav 
a nad tím provedeš další move... (DFS)
*/

% -------------------------------------------------------------------------
% Main function of the program
main:-
	prompt(_, ''),
	read_lines(LL),
	split_lines(LL,S),
	map_cube(S,C),
	print_cube(C),
	halt.