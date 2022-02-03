ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

% 1. size(BT, N). N is the number of integer-labelled nodes in BT.

size(empty, 0).
size(node(_, L, R), N):- size(L, N1), size(R, N2), N is N1+N2+1.    %is predicate is used for evaluation of arithmetic expressions and assigning them to variables

%size(BT, N):- BT=_(_, L, R), size(L, N1), size(R, N2), N=N1+N2+1.  Was trying this way earlier, but realised that assignment does not happen like this in prolog

% 2. height(BT, N). N is the height of BT with empty havinNg a height of 0.

% a predicate for the maximum of two numbers
max(A, B, A):- integer(A), integer(B), A>=B.
max(A, B, B):- integer(A), integer(B), B>=A.

height(empty, 0).
height(node(_, L, R), N):- height(L, N2), height(R, N3), max(N2, N3, N1), N is N1+1 .       %the height is one greater than the maximum of heights of the left and the right subtree


% 3. preorder(BT, L). L is the preorder traversal of BT.
preorder(empty, []).
preorder(node(N, Lf, R), L):- preorder(Lf, L1), preorder(R, L2), append([N], L1, L3), append(L3, L2, L).        % Root Left Right

% 4. inorder(BT, L). L is the inorder traversal of BT.
inorder(empty, []).
inorder(node(N, Lf, R), L):- inorder(Lf, L1), inorder(R, L2), append(L1, [N], L3), append(L3, L2, L).           %Left Root Right


% 5. postorder(BT, L). L is the postorder traversal of BT.
postorder(empty, []).
postorder(node(N, Lf, R), L):- postorder(Lf, L1), postorder(R, L2), append(L1, L2, L3), append(L3, [N], L).     %Left Right Root

% 6. trPreorder(BT, L). L is a tail-recursive preorder traversal of BT.

% predicate to delete the first element of a list
delfirst([X], [], X).
delfirst([X | L], L, X).

%predicate to give the left child node of a BT
giveLf(node(_, empty, _), empty).
giveLf(node(_, Lf, _), Lf).

%predicate to give the value of a node
giveval(node(X, _, _), X).

%prehelp(BT, L1, L):- takes a Binary Tree BT , the list L1 of nodes visited until now, and stores the preorder traversal in L (since we store all left children first in L1 and when we reach the last left node then we look at the corresponding right node)

prehelp(empty, [], []).
prehelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, node(_, _, empty)), reverse(L3, L4), prehelp(empty, L4, L).
prehelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, node(_, _, R)), reverse(L3, L4), prehelp(R, L4, L) .
prehelp(BT, L1, L):-  append(L1, [BT], L2), giveLf(BT, Lf), giveval(BT, X), prehelp(Lf, L2, L4), append([X], L4, L).

trpreorder(empty, []).
trpreorder(BT, L):- ibt(BT), prehelp(BT, [], L).

% 7. trInorder(BT, L). L is a tail-recursive inorder traversal of BT.

%inhelp(BT, L1, L):- takes a Binary Tree BT , the list L1 of nodes visited until now, and stores the inorder traversal in L

inhelp(empty, [], []).
inhelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, node(X, _, empty)), reverse(L3, L4), inhelp(empty, L4, L6), append([X], L6, L) .
inhelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, node(X, _, R)), reverse(L3, L4), inhelp(R, L4, L6), append([X], L6, L) .

inhelp(BT, L1, L):-  append(L1, [BT], L2), giveLf(BT, Lf), inhelp(Lf, L2, L).

trinorder(empty, []).
trinorder(BT, L):- ibt(BT), inhelp(BT, [], L).




% 8. trPostorder(BT, L). L is a tail-recursive postorder traversal of BT.



%predicate to give the right child node of a BT
giveR(node(_, _, empty), empty).
giveR(node(_, _, R), R).

checkfirst(X, [X | _]) .        %to check if the element is the first element of a given list

%posthelp(BT, L1, L):- takes a Binary Tree BT , the list L1 of nodes visited until now, and stores the postorder traversal in L


posthelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, BT), giveR(BT, R), giveval(BT, _), checkfirst(R, L3), delfirst(L3, L4, _), reverse(L4, L5), append(L5, [BT], L6), posthelp(R, L6, L) .
posthelp(empty, L1, L):- reverse(L1, L2), delfirst(L2, L3, node(X, _, empty)), reverse(L3, L4), posthelp(empty, L4, L6), append([X], L6, L) .

posthelp(BT, L1, L):-  giveLf(BT, Lf), giveR(BT, R), append(L1, [R], L2), append(L2, [BT], L3), posthelp(Lf, L3, L).
posthelp(_, [], []).

trpostorder(empty, []).
trpostorder(BT, L):- ibt(BT), posthelp(BT, [], L).



% 9. eulerTour(BT, L). L is the Euler tour of BT. 
eulerTour(empty, []).
eulerTour(node(N, Lf, R), L):- Lf==empty, R==empty, L=[N, N, N], !.
eulerTour(node(N, Lf, R), L):- Lf==empty, L1=[N, N], eulerTour(R, L2), append(L1, L2, L3), append(L3, [N], L).
eulerTour(node(N, Lf, R), L):- R==empty, eulerTour(Lf, L1), append([N], L1, L2), append(L2, [N, N], L).
eulerTour(node(N, Lf, R), L):- eulerTour(Lf, L1), eulerTour(R, L2), append([N], L1, L3), append(L3, [N], L4), append(L4, L2, L5), append(L5, [N], L). 


% 10. preET(BT, L). L is the preorder traversal of BT extracted from the Euler tour.

%predicate for last element of a list
last(X,[X]).
last(X,[_|Z]) :- last(X,Z).     %Alternative definition -> last(X,Y) :-append(_,[X],Y).

%rmxl(X, L1, L) takes in the current element X ,a list L1, and gives the list L after removing all occurences of X in the list L1
rmxl(X, [X], []).
rmxl(_,[], []).
rmxl(X, [X|L1], L):- rmxl(X, L1, L).
rmxl(X, [Y|L1], L):- rmxl(X, L1, L2), append([Y], L2, L).

%rmdup(L, L1) converts euler tour L1 to preorder traversal L by simply removing duplicates of all elements
rmdup([], []).
rmdup([X], [X]).
rmdup([X | L1], L):- rmxl(X, L1, L2), rmdup(L2, L3), append([X], L3, L).


preET(empty, []).
preET(BT, L):- ibt(BT), eulerTour(BT, L1),rmdup(L1, L).

% 11. inET(BT, L). L is the inorder traversal of BT extracted from the Euler tour.

inET(empty, []).
inET(BT, L):- ibt(BT), eulerTour(BT, L1),rmdup(L1, L2), sort(L2, L).        %using the fact that the inorder traversal is essentially the sorted order of the node values

% 12. postET(BT, L). L is the postorder traversal of BT extracted from the Euler tour.

postET(empty, []).
postET(BT, L):- ibt(BT), eulerTour(BT, L1), reverse(L1, L2), rmdup(L2, L3), reverse(L3, L).     % reversing the euler tour then removing duplicates and then reversing the list obtained to get the postorder traversal

% 13. toString(BT, S). S is the string representing a linear rendering of a nonempty binary tree BT as a 3-
% tuple of the form “(N, LBT, RBT)” where N is the label of the root and LBT and RBT are the left and right
% subtrees respectively. An empty tree is rendered as “()”.

toString(empty, "()").
toString(node(N, L, R), S):- string_concat("(", N, S1), string_concat(S1, ", ", S2), toString(L, S3), toString(R, S4), string_concat(S2, S3, S5), string_concat(S5, ", ", S6), string_concat(S6, S4, S7), string_concat(S7, ")", S).

% 14. isBalanced(BT). Whether BT is balanced.

isBalanced(empty).
isBalanced(node(_, L, R)):- height(L, H1), height(R, H2), H1 is H2-1, isBalanced(L), isBalanced(R).     %by definition of a balanced BT
isBalanced(node(_, L, R)):- height(L, H1), height(R, H2), H1 is H2, isBalanced(L), isBalanced(R).
isBalanced(node(_, L, R)):- height(L, H1), height(R, H2), H1 is H2+1, isBalanced(L), isBalanced(R).

% 15. isBST(BT). Whether BT is a binary search tree under the usual < ordering on integers.

% Was trying an alternate approach which didn't work
/*isBST(node(N, n1(N1, _, _), empty)):- N1 < N, isBST(n1).
isBST(node(N, empty, n2(N1, _, _))):- N1 > N, isBST(n2).
*/

% less(N, BT)  ->  a predicate to check if all elements in a BT are less than a given element N

less(_, empty).
less(N, node(N1, L, R)):- N > N1, less(N, L), less(N, R).

% greater(N, BT)  ->  a predicate to check if all elements in a BT are greater than a given element N
greater(_, empty).
greater(N, node(N1, L, R)):- N < N1, greater(N, L), greater(N, R).


isBST(empty).
isBST(node(_, empty, empty)).
isBST(node(N, L, R)):- integer(N), isBST(L), isBST(R), less(N, L), greater(N, R).       %by definition of a BST

% 16. makeBST(L, BST). BST is a balanced binary search tree whose nodes have labels from L.

createnode(X, node(N,L,R)):- N is X, L is empty, R is empty.    %creates a leaf node with the value X


makeBST([], empty).
makeBST([N], node(N, empty, empty)).
makeBST([X|L], BST):-makeBST(L, BST1), insert(X, BST1, BST) .       %insert elements one by one into the BST


% 17. lookup(N, BST). Determine whether there is a node labelled N in BST.

lookup(N, node(N, _, _)).
lookup(N, node(_, L, _)):- lookup(N, L).        %look for the node in the left subtree
lookup(N, node(_, _, R)):- lookup(N, R).        %look for the node in the right subtree

% 18. insert(N, BST1, BST2). Insert a new node labelled N in BST1 to yield BST2 provided there is no node
% labelled N already in BST1.

/*set(X, empty, R, node(N, L1, R1)):- N = X, L1 = empty, R1 = R.
set(X, L, empty, node(N, L1, R1)):- N = X, L1 = L, R1 = empty.
set(X, L, R, node(N, L1, R1)):- N = X, L1 = L, R1 = R.
*/
insert(N, empty, node(N, empty, empty)).
/*insert(X, node1(N, empty, empty), node2(N, empty, R2)):- X > N, insert(X, empty, R2).
insert(X, node1(N, empty, empty), node2(N, L, empty)):- X < N, insert(X, empty, L).*/
insert(X, node(N, L1, R), node(N, L2, R)):- X<N, insert(X, L1, L2).     %if the node to be inserted has value less than the value of the root node, then it should be inserted in the left subtree  (by definition of BST)
insert(X, node(N, L, R1), node(N, L, R2)):- X>N, insert(X, R1, R2).     %if the node to be inserted has value less than the value of the root node, then it should be inserted in the right subtree  (by definition of BST)


/*
A failed attempt
insert(N, node(N, _, _), node).
insert(N, node1(X, empty, empty), BST):- set(N, empty, empty, L), set(X, L, empty, BST).
insert(N, node1(X, empty, empty), BST):- set(N, empty, empty, R), set(X, empty, R, BST).
%insert(N, node1(X, L, empty), node2(X, L, N)).
%insert(N, node1(X, empty, R), node2(X, N, R)).
insert(N, node1(X, L, R), BST2):- insert(N, L, L1), set(X, L1, R, BST2), isBST(BST2).
insert(N, node1(X, L, R), BST2):- insert(N, R, R1), set(X, L, R1, BST2), isBST(BST2).

*/

% 19. delete(N, BST1, BST2). Delete a node labelled N (if it is present) from BST1 to yield BST2.

% makelist(L, BST) is a predicate which makes a list of values of nodes of a given BST. 
makelist([], empty).
makelist(L, node(N, Lf, R)):- makelist(L1, Lf), makelist(L2, R), append(L1, L2, L3), append(L3, [N], L).

%Delete element in List predicate

delele(X, [X | L], L).
delele(X, [Y | L], L1):- delele(X, L, L2), append([Y], L2, L1).
delete(N, BST1, BST2):-lookup(N, BST1), makelist(L, BST1), delele(N, L, L1), makeBST(L1, BST2).      %Make a list of node values, then delete the given node value from the list, now make a BST from this list 
