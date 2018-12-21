
%% transpose(+Mat: 2D Matrix, -RemMat: Transposed 2D matrix)
%
%
transpose([[]|_], []).
transpose(Mat, [HRow|TRows]) :-
	transpose_col_1(Mat, HRow, RemMat),
	transpose(RemMat, TRows).


%% transpose_col_1
% arg 1 : Matrix
% arg 2: Resultant transposed row 
% arg 3: remainder of matrix with first column removed
%
%
transpose_col_1([],[],[]).
transpose_col_1([[H|T]| TRows], [H|HT], [T|TT]) :-
	transpose_col_1(TRows, HT, TT).

%% rowLen(+List:list to check row length, +N:integer of length)
%
%
rowLen([],_).
rowLen([HRow|TRow], N) :-
	length(HRow, N),
	rowLen(TRow, N).

%% lapply(+Fun: function to apply, +List:list, -list with 
%	fun applied)
%
%  arity 1
%
lapply(_,[]).
lapply(Fun, [H|T]) :-
	call(Fun, H),
	lapply(Fun, T).


% arity 2
%
%
mapList(_,[],[]).
mapList(Fun, [H1|T1], [H2|T2]) :-
	call(Fun, H1, H2),
	mapList(Fun, T1, T2).

%% domain
%
%
domain(N, X) :- 
	fd_domain(X, 1, N).


%% towerViewCount(+Row:list, +TowerCount:integer)
%
%
% Goes all the way down, then only increments count is the next number in the
% list is greater than itself.
%
towerViewCount([H|T], TowerCount) :-
	towerViewCount([H|T], H, TowerCount).

towerViewCount([], _, 1).
towerViewCount([NH|NT], CurMax, TowerCount) :-
	(NH #> CurMax ->
	towerViewCount(NT, NH, M),
	TowerCount is M+1
	;
	towerViewCount(NT, CurMax, TowerCount)).


%% tower(+N: integer, +T:list of lists, -C:list of lists)
%
% General mentality:
% Run towercount on rows, then reversed rows
% transponse T and run again.
%
%
tower(N, T, C) :-
	% set counts
	C = counts(Top, Bot, Left, Right),
	% checks
	length(T, N),
	rowLen(T, N),
	lapply(domain(N), T),
	lapply(fd_all_different, T),
	transpose(T, TransT),
	lapply(domain(N), TransT),
	lapply(fd_all_different, TransT),
	lapply(fd_labeling, T),

	% Check lens
	length(Top, N),
	length(Bot, N),
	length(Left, N),
	length(Right, N),

	% calculate/verify tower view counts
	mapList(towerViewCount, T, Left),
	mapList(reverse, T, RevT),
	mapList(towerViewCount, RevT, Right),
	% perform on transposed matrix
	mapList(towerViewCount, TransT, Top),
	mapList(reverse, TransT, RevTransT),
	mapList(towerViewCount, RevTransT, Bot).


%% checkBounds(+List, +Lower:Integer, +Upper:Integer)
%
%  Checks if each elm within list is between the bounds.
%
%
checkBounds([], _, _).
checkBounds([H|T], Lower, Upper) :-
	H #>= Lower,
	H #=< Upper,
	checkBounds(T, Lower, Upper).


%% plainDomain(+N: Integer, +X:list)
%
%
plainDomain(N, X) :-
	checkBounds(X, 1, N).


%% diffChecks(+Junk:list, +H:head item, +List:list to check through)
%
%
diffChecks(_, _, []).
diffChecks(Junk, H, [TH|TT]) :-
	H #\= TH,
	diffChecks(Junk, H, TT),
	append(Junk, [H], NewJunk),
	diffChecks(NewJunk, TH, TT).

%% plainDifferent(+List: list to check for any duplicate elms)
%
%
plainDifferent([H|T]) :-
	diffChecks([], H, T). 


%% enumerate(+N: integer, -L:list)
%
%  enumerate all possible integer solution.
%  Utilize SWI-Prolog -- permutation/2
%  
enumerate(N, L) :-
	findall(TEMP, between(1, N, TEMP), X), 
	permutation(X, L).



%% plain_tower(+N: integer, +T:list of lists, -C:list of lists)
%
%
plain_tower(N, T, C) :-
	% set counts
	C = counts(Top, Bot, Left, Right),
	length(T, N),
	rowLen(T, N),
	lapply(plainDomain(N), T),
	lapply(plainDifferent, T),
	transpose(T, TransT),
	lapply(plainDomain(N), TransT),
	lapply(plainDifferent, TransT),
	lapply(enumerate(N), T),

	% Check lens
	length(Top, N),
	length(Bot, N),
	length(Left, N),
	length(Right, N),

	% now verify
	mapList(towerViewCount, T, Left),
	mapList(reverse, T, RevT),
	mapList(towerViewCount, RevT, Right),
	mapList(towerViewCount, TransT, Top),
	mapList(reverse, TransT, RevTransT),
	mapList(towerViewCount, RevTransT, Bot).


%% speedup(-Ratio)
%
%  Divides the time it takes for plain_tower to complete
%  over tower for a specified test case
%
%
speedup(Ratio) :- 
	statistics(cpu_time, [Start1|_]),
	tower(5, __T1, counts([5,2,1,3,2],[1,2,4,2,2],[3,4,3,2,1],[2,1,2,3,2])),
	statistics(cpu_time, [Stop1|_]),
	TSpeed is Stop1 - Start1,
	statistics(cpu_time, [Start2|_]),
	plain_tower(5, __T2, counts([5,2,1,3,2],[1,2,4,2,2],[3,4,3,2,1],[2,1,2,3,2])),
	statistics(cpu_time, [Stop2|_]),
	PTSpeed is Stop2 - Start2, 
	Ratio is PTSpeed/TSpeed.

%% ambiguous(+N: integer, +C:counts([]*4), -T1:Solution1, -T2:Solution2)
%
%  Checks for whether a particular set of counts is ambiguous
%  meaning there are multiple possible solutions.
%
%
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.
