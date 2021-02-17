% Lists

% P01
% Find last element in list
last([X], X).
last([_|R], X) :- last(R, X).

% P02
% Find second-last element in list
secondLast([X,_], X).
secondLast([_|R], X) :- secondLast(R, X).

% P04
% Find the number of elements in a list
length_(L, N) :- foldl([_,Y,Z]>>(Z is Y+1), L, 0, N).

% P05
% Reverse a list
reverse(L, R) :- foldl([X,Y,Z]>>append([X], Y, Z), L, [], R).

% P06
% Test if list is palindrome
palindrome(L) :-
  reverse(L, R),
  L = R.

% P07
% Flatten a nested list
flatten([], []).
flatten([X|R], L) :-
  is_list(X), !,
  flatten(X, X1),
  flatten(R, R1),
  append(X1, R1, L).
flatten([X|R], [X|L]) :-
  flatten(R, L).

% P09
% Duplicate elements in list
dup([], []).
dup([X|R], [X,X|L]) :- dup(R, L).

% P17
% Split list at index k
split(L, 0, [], L).
split([X|R], N, [X|L1], L2) :-
  N1 is N-1,
  split(R, N1, L1, L2).

% P22
% Fill list with numbers from N to K, both inclusive, K > N.
range(N, K, []) :- N > K.
range(N, K, [N|L]) :-
  N =< K,
  N1 is N+1,
  range(N1, K, L).

% Binary Trees

% P54
% Check whether a given term is a binary tree
% Empty Tree = nil
% Non-empty Tree = tree(T1, X, T2)
%   where T1 and T2 are subtrees.
isTree(nil).
isTree(tree(T1, _, T2)) :-
  isTree(T1),
  isTree(T2).

% P57
% Construct a binary search tree from list
construct(L, T) :- foldl([X,Y,Z]>>insert(Y, X, Z), L, nil, T).

insert(nil, X, tree(nil, X, nil)).
insert(tree(T1, X, T2), X, tree(T1, X, T2)).
insert(tree(T1, Y, T2), X, tree(T3, Y, T2)) :-
  X < Y,
  insert(T1, X, T3).
insert(tree(T1, Y, T2), X, tree(T1, Y, T3)) :-
  X > Y,
  insert(T2, X, T3).
