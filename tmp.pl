%kthRow(+Matrix, +Row, -Res)
kthRow([H|_], 1, [H]) :- !.
kthRow([_|T], L, R) :- M is L - 1, kthRow(T,M,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%sumRev(+Array1, +Array2, -Res)
sumRev(F,S,R) :- sum(F,S,X), revA(X,[],R).

sum([], S, S) :- !.
sum(F, [], F) :- !.
sum([F|R1],[S|R2], [R|Rn]) :-
    R is F+S, 
    sum(R1,R2,Rn).

revA([],Acc,Acc).
revA([H|T],Acc,R):- revA(T,[H|Acc],R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
minList([H|T],R) :- minA(T,H,R).

minA([],Acc,Acc).
minA([H|T], Acc, R) :- H < Acc, !, minA(T, H, R).
minA([_|T], Acc, R)   :-  minA(T,Acc,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% v1
eoVal(L,R) :- odds(L,[],O), evens(L,[],E), revA(O,[],RO),
		revA(E,[],RE), append(RE,RO,R).

odds([],Acc,Acc).
odds([H|T],Acc,R) :- 1 =:= ( H mod 2), !, odds(T,[H|Acc],R).
odds([_|T],Acc,R) :-  odds(T,Acc,R).

evens([],Acc,Acc).
evens([H|T],Acc,R) :- 0 =:= ( H mod 2), !, evens(T,[H|Acc],R).
evens([_|T],Acc,R) :-  evens(T,Acc,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%v2
eoV(L,R) :- evn(L,E),odd(L,O), my_append(E,O,R).
evn([],[]).
evn([H|T],[H|R]) :- 0 =:= ( H mod 2), !,evn(T,R).
evn([H|T],R) :- 1 =:= ( H mod 2), !,evn(T,R).

odd([],[]).
odd([H|T],[H|R]) :- 1 =:= ( H mod 2), !,odd(T,R).
odd([H|T],R) :- 0 =:= ( H mod 2), !,odd(T,R).

% my_append2(+Lst, +Elem, -Res).
my_append2([], E, [E]).
my_append2([H|T], E, [H|Res]) :- my_append(T, E, Res).

% my_append(+Lst1, +Lst2, -Res).
my_append([], Lst2, Lst2).
my_append([H|T], Lst2, [H|Res]) :- my_append2(T, Lst2, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cnt(L,N,R) :- cntA(L,N,0,R).
cntA([],_,Acc,Acc).
cntA([H|T],H,Acc,R) :- !, X is Acc+1, cntA(T,H,X,R).
cntA([H|T],N,Acc,R) :- H \= N, cntA(T,N,Acc,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mulRev(L1,L2,R) :- mulA(L1,L2,X), revA(X,[],R).
mulA(F,[],F) :- !.
mulA([],S,S) :- !.
mulA([H1|T1],[H2|T2],[X|R]) :- X is H1 * H2, mulA(T1,T2,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%BST-insert
bst_insert(N,nil,bst(N,nil,nil)):-!.
bst_insert(N,bst(V,L,R),bst(V,L,X)) :- N > V, !, bst_insert(N,R,X).
bst_insert(N,bst(V,L,R),bst(V,X,R)) :- N < V, !, bst_insert(N,L,X).
%BST-create
bst_create([], nil).
bst_create([H|T], R):- bst_create(T,Acc), bst_insert(H,Acc,R).
%BST-inorder
bst_inorder(nil,[]).
bst_inorder(bst(V,L,R),Res):-bst_inorder(L,LR),bst_inorder(R,RR),
    my_append2(LR,V,X),my_append2(X,RR,Res).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%print1st
printFirstColumn([],[]).
printFirstColumn([[H|_]|T],R):- printFirstColumn(T,R1), R=[H|R1].
%delete1st
deleteFirstColumn([],[]).
deleteFirstColumn([[_|T1]|T],R):-deleteFirstColumn(T,R1), R=[T1|R1].
%transponse
transponse([],[]):-!.
transponse([[]|T],Res):-!,transponse(T,Res).
transponse(L,Res):- printFirstColumn(L,P1), deleteFirstColumn(L,D1),
    transponse(D1,X), Res = [P1|X].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% my_fact(+N, -Res).
my_fact(0, 1) :- !.
my_fact(N, Res) :- N1 is N - 1, my_fact(N1, Res1), Res is N * Res1.

% my_fib(+N, -Res).
my_fib(0, 1) :- !.
my_fib(1, 1) :- !.
my_fib(N, Res) :- N1 is N-1, N2 is N-2, my_fib(N1, R1), my_fib(N2, R2), Res is R1 + R2.

% -----------------------------------------------------------------------------
% 2

% my_contains(+Lst, +Elem).
my_contains([H|_], H) :- !.
my_contains([H|T], E) :- H \= E, my_contains(T, E).


% my_not_contains(+Lst, +Elem).
my_not_contains([], _).
my_not_contains([H|_], H) :- fail.
my_not_contains([H|T], E) :- H \= E, my_not_contains(T, E).

% my_nth(+Lst, +N, -Res).
my_nth([H|_], 0, H) :- !.
my_nth([_|T], N, Res) :- N1 is N-1, my_nth(T, N1, Res).
% Q: proc funguje i pro my_nth([0], 1, R) ?

% my_length(+Lst, -Res).
my_length([], 0).
my_length([_|T], Res) :- my_length(T, R1), Res is 1 + R1.

% my_length2(+Lst, -Res).
my_length2([], 0) :- !.
my_length2([H|T], Res) :- is_list(H), my_length2(H, R1), my_length2(T, R2), Res is R1 + R2, !.
my_length2([_|T], Res) :- my_length2(T, R1), Res is 1 + R1.

% my_append(+Lst, +Elem, -Res).
my_append([], E, [E]).
my_append([H|T], E, [H|Res]) :- my_append(T, E, Res).

% my_append2(+Lst1, +Lst2, -Res).
my_append2([], Lst2, Lst2).
my_append2([H|T], Lst2, [H|Res]) :- my_append2(T, Lst2, Res).

% my_delete_first(+Lst, +E, -Res).
my_delete_first([], _, []).
my_delete_first([H|T], H, T) :- !.
my_delete_first([H|T], E, [H|Res]) :- H \= E, my_delete_first(T, E, Res).

% my_delete_all(+Lst, +E, -Res).
my_delete_all([], _, []).
my_delete_all([H|T], H, Res) :- my_delete_all(T, H, Res), !.
my_delete_all([H|T], E, [H|Res]) :- H \= E, my_delete_all(T, E, Res).

% my_replace(+Lst, +S, +R, -Res).
my_replace([], _, _, []).
my_replace([S|T], S, R, [R|Res]) :- my_replace(T, S, R, Res), !.
my_replace([H|T], S, R, [H|Res]) :- H \= S, my_replace(T, S, R, Res).

% my_last(+Lst, -Res).
my_last([H], H) :- !.
my_last([_|T], Res) :- my_last(T, Res).

% my_count(+E, +Lst, -Cnt)
my_count(_, [], 0) :- !.
my_count(E, [H|T], Cnt)  :- E \= H, my_count(E, T, Cnt), !.
my_count(E, [E|T], Cnt2) :-         my_count(E, T, Cnt), Cnt2 is Cnt + 1.

% my_flatten(+Lst, -Res).
my_flatten([], []).
my_flatten([H|T], Res) :- is_list(H), my_flatten(H, Res1), my_flatten(T, Res2), my_append2(Res1, Res2, Res), !.
my_flatten([H|T], [H|Res]) :- my_flatten(T, Res).

% my_reverse(+Lst, -Res).
my_reverse(Lst, Res) :- my_reverse_aux(Lst, [], Res).
my_reverse_aux([], Acc, Acc).
my_reverse_aux([H|T], Acc, Res) :- my_reverse_aux(T, [H|Acc], Res).

% my_is_sorted(+Lst). - option 1
my_is_sorted(Lst) :- my_is_sorted_asc(Lst).
my_is_sorted(Lst) :- my_is_sorted_dsc(Lst).
my_is_sorted_asc([]).
my_is_sorted_asc([_]) :- !.
my_is_sorted_asc([H1, H2 | T]) :- H1 =< H2, my_is_sorted_asc([H2|T]).
my_is_sorted_dsc([]).
my_is_sorted_dsc([_]) :- !.
my_is_sorted_dsc([H1, H2 | T]) :- H1 >= H2, my_is_sorted_asc([H2|T]).

% my_is_sorted(+Lst). - option 2
my_is_sorted2(Lst) :- my_is_sorted_asc(Lst), !.
my_is_sorted2(Lst) :- my_reverse(Lst, LstRev), my_is_sorted_asc(LstRev).

% my_powerset(+Lst, -Res).
% TODO

% -----------------------------------------------------------------------------
% 3

%my_factlist(+N, -Res).
my_factlist(0, [1]) :- !.
my_factlist(N, [1|Res]) :- my_factlist_aux(1, N, 1, Res).
my_factlist_aux(N, N, Fact, [Fact]) :- !.
my_factlist_aux(I, N, Fact, [Fact|Res]) :- NI is I + 1, NFact is NI * Fact, my_factlist_aux(NI, N, NFact, Res).

%my_fiblist(+N, -Res).
% TODO

% -----------------------------------------------------------------------------
% 4

my_zero([]).

my_successor([], [x]).
my_successor([x|T], [x,x|T]).

my_add([], Num, Num).
my_add([x|T], Num, [x|Res]) :- my_add(T, Num, Res).

my_mul([], _, []).
my_mul([x], Num, Num) :- !.
my_mul([x|T], Num, Res) :- my_mul(T, Num, Res1), my_add(Res1, Num, Res).

% -----------------------------------------------------------------------------
% 5

my_counter([], []).
my_counter([H|T], [[H,Cnt] | Res]) :- my_count(H, [H|T], Cnt), my_delete_all(T, H, Lst2), my_counter(Lst2, Res).

% -----------------------------------------------------------------------------
% 6

% my_split(+Lst, -L1, -L2).
my_split(Lst, L1, L2) :- my_length(Lst, Len), Len2 is Len // 2, my_split(Lst, 0, Len2, L1, L2).

my_split(Lst, N, N, [], Lst) :- !.
my_split([H|T], I, N, [H|L1], L2) :- I1 is I + 1, my_split(T, I1, N, L1, L2).

% my_merge(+L1, +L2, -Res).
my_merge(L1,      [],      L1) :- !.
my_merge([],      L2,      L2) :- !.
my_merge([H1|T1], [H2|T2], [H1|Res]) :- H1 =< H2, my_merge(T1, [H2|T2], Res), !.
my_merge([H1|T1], [H2|T2], [H2|Res]) :- H1 >  H2, my_merge([H1|T1], T2, Res), !.

% my_mergesort(+Lst, -Sorted).
my_mergesort([], []) :- !.
my_mergesort([H], [H]) :- !.
my_mergesort(Lst, Sorted) :- my_split(Lst, L1, L2), my_mergesort(L1, R1), 
	my_mergesort(L2, R2), my_merge(R1, R2, Sorted).


% my_selectpivot(+Lst, -E).
my_selectpivot([], _) :- fail.
my_selectpivot([H|_], H).

%my_split_by_pivot(+Lst, +Pivot, -Lt, -Eq, -Gt).
my_split_by_pivot([], _, [], [], []).
my_split_by_pivot([H|T], Pivot, [H|Lt], Eq, Gt) :- H < Pivot, my_split_by_pivot(T, Pivot, Lt, Eq, Gt), !.
my_split_by_pivot([H|T], Pivot, Lt, [H|Eq], Gt) :- H = Pivot, my_split_by_pivot(T, Pivot, Lt, Eq, Gt), !.
my_split_by_pivot([H|T], Pivot, Lt, Eq, [H|Gt]) :- H > Pivot, my_split_by_pivot(T, Pivot, Lt, Eq, Gt), !.

% my_quicksort(+Lst, -Sorted).
my_quicksort([], []).
my_quicksort(Lst, Sorted) :- my_selectpivot(Lst, Pivot), my_split_by_pivot(Lst, Pivot, L1, L2, L3), my_quicksort(L1, S1), my_quicksort(L3, S3), my_append2(S1, L2, Sorted1), my_append(Sorted1, S3, Sorted).

% -----------------------------------------------------------------------------
% BST-1

% bst = [ ]
% bst = [ val left right ]

% my_bstfind(+BST, -E).
my_bstfind([], _) :- fail.
my_bstfind([E, _, _], E) :- !.
my_bstfind([K, L, _], E) :- E < K, my_bstfind(L, E), !.
my_bstfind([K, _, R], E) :- E > K, my_bstfind(R, E), !.

% my_bstinsert(+BST, +E, -NBST).
my_bstinsert([], E, [E, [], []]).
my_bstinsert([E, _, _], E, _) :- fail.
my_bstinsert([K, L, R], E, [K, NBST, R]) :- E < K, my_bstinsert(L, E, NBST), !.
my_bstinsert([K, L, R], E, [K, L, NBST]) :- E > K, my_bstinsert(R, E, NBST), !.

% my_bstinorder(+BST, -Lst).
my_bstinorder([], []).
my_bstinorder([K, L, R], Res) :- my_bstinorder(L, RL), my_bstinorder(R, RR), my_append2(RL, [K], Res1), my_append2(Res1, RR, Res).

% -----------------------------------------------------------------------------
% BST-2

% my_treesort(+List, -Res).
my_treesort(Lst, Res) :- my_bstconstruct(Lst, Bst), my_bstinorder(Bst, Res).

% my_bstconstruct(+Lst, -Bst).
my_bstconstruct([], []).
my_bstconstruct([H|T], Bst) :- my_bstconstruct(T, BstTmp), my_bstinsert(BstTmp, H, Bst).

% -----------------------------------------------------------------------------
% MAT-1

% my_mat_dimensions(Mat, -Rows, -Cols).
my_mat_dimensions([H|T], Rows, Cols) :- my_length(H, Cols), my_length([H|T], Rows).

% my_mat_nth_row(+Mat, +N, -Res)
my_mat_nth_row(Mat, N, Res) :- my_nth(Mat, N, Res).

% my_mat_nth_col(+Mat, +N, -Res)
my_mat_nth_col([], _, []).
my_mat_nth_col([Row|T], N, [NthElem|Res]) :- my_nth(Row, N, NthElem), my_mat_nth_col(T, N, Res).

% my_mat_diag(+Mat, -Res)
not(P) :- P, !, fail.
not(_).

my_mat_diag(Mat, Res) :- my_mat_diag(Mat, 0, Res).
my_mat_diag([], _, []).
my_mat_diag([Row|_], N, []) :- not(my_nth(Row, N, _)), !.
my_mat_diag([Row|T], N, [Elem|Res]) :- my_nth(Row, N, Elem), N1 is N + 1, my_mat_diag(T, N1, Res).

% my_mat_rem_first_col(+Mat, +Res)
my_remove_first([_|T], T).
my_mat_rem_first_col([], []).
my_mat_rem_first_col([H|T], [HR|Res]) :- my_remove_first(H, HR), my_mat_rem_first_col(T, Res).

% my_mat_transpose(+Mat, -Res).
my_mat_transpose([H|T], []) :- my_length(H, 0), !.
my_mat_transpose(Mat, [Col|Res]) :-
	my_mat_nth_col(Mat, 0, Col), % collect first col
	my_mat_rem_first_col(Mat, Mat2), % create matrix without first col
	my_mat_transpose(Mat2, Res). % transpose the rest

















