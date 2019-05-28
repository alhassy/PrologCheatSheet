%% [[file:~/PrologCheatSheet/CheatSheet.org::*Homemade%20Interactive%20Prolog%20Setup][Homemade Interactive Prolog Setup:4]]
magicNumber(7).
magicNumber(9).
magicNumber(42).

% ?- magicNumber(8).
% ?- magicNumber(X).
%% Homemade Interactive Prolog Setup:4 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Homemade%20Interactive%20Prolog%20Setup][Homemade Interactive Prolog Setup:5]]
main :- write('Hello, world!').

% ?- main.
%% Homemade Interactive Prolog Setup:5 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Facts%20---Nullary%20Relations][Facts ---Nullary Relations:1]]
jasim_is_nice.

% ?- jasim_is_nice. %⇒ true: We declared it so.

it_is_raining. /* Another fact of our world */

% ?- it_is_raining. %⇒ true

eats(fred, mangoes).
eats(bob, apples).
eats(fred, oranges).

% ?- eats(bob, apples). %⇒ true

% Which foods are eaten by fred?
% ?- eats(fred, what). %⇒ false; ‘what’ is name!
% ?- eats(fred, What). %⇒ mangoes oranges
%% Facts ---Nullary Relations:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Facts%20---Nullary%20Relations][Facts ---Nullary Relations:2]]
% All men are mortal.
mortal(X) :- man(X).

% Socrates is a man.
man(socrates).

% Hence, he's expected to be mortal.
% ?- mortal(socrates). %⇒ true

% What about Plato?
% ?- mortal(plato). %⇒ false, plato's not a man.

% Let's fix that.
man(plato).

% Who is mortal?
% ?- mortal(X). % ⇒ socrates plato
%% Facts ---Nullary Relations:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Conjunction][Conjunction:1]]
yum(pie).
yum(apples).
yum(maths).

% ?- yum(Y), writeln(Y), fail. %⇒ pie apples maths false.
%% Conjunction:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Arithmetic%20with%20~is~][Arithmetic with ~is~:1]]
% ?- X = 3 + 2.  %% X = 3 + 2
% ?- X is 3 + 2. %% X = 5
%% Arithmetic with ~is~:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Declaration%20Ordering%20Matters][Declaration Ordering Matters:1]]
% Graph
edge(a, b). edge(b ,c). edge(c, d).

% Works
path(X, X).
path(X, Y) :- edge(Z, Y), path(X, Z).
% ?- path(a, d). %⇒ true.

% Fails: To find a path, we have to find a path, before an edge!
% The recursive clause is first and so considerd before the base clause!
path_(X, Y) :- path_(X, Z), edge(Z, Y).
path_(X, X).
% ?- path_(a, d). %⇒ loops forever!
%% Declaration Ordering Matters:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*ADT:%20Pairs,%20Numbers,%20Lists,%20and%20Trees][ADT: Pairs, Numbers, Lists, and Trees:1]]
% In Haskell: Pair a b = MkPair a b

pair(_, _).

% ?- pair(1, "nice").
% ?- pair(1, "nice") = pair(A, "nice"). %⇒ A = 1

% In Haskell: Nat = Zero | Succ Nat

nat(zero).
nat(succ(N)) :- nat(N).

% ?- nat(succ(succ(zero))).

sum(zero, N, N).
sum(succ(M), N, succ(S)) :- sum(M, N, S).

% ?- Two = succ(succ(zero)), Four = succ(succ(succ(succ(zero)))), sum(Two, Two, Four).
%% ADT: Pairs, Numbers, Lists, and Trees:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*ADT:%20Pairs,%20Numbers,%20Lists,%20and%20Trees][ADT: Pairs, Numbers, Lists, and Trees:2]]
% In Haskell: Tree a = Leaf a | Branch (Tree a) (Tree a)

tree(leaf(_)).
tree(branch(L, R)) :- tree(L), tree(R).

% ?- A = leaf(1), B = leaf(2), L = branch(A, B), R = branch(A, A), tree(branch(L, R)).
%% ADT: Pairs, Numbers, Lists, and Trees:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*ADT:%20Pairs,%20Numbers,%20Lists,%20and%20Trees][ADT: Pairs, Numbers, Lists, and Trees:3]]
% Head: (car (cons X Xs)) = X
% Tail: (cdr (cons X Xs)) = Xs
% Extensionality: (cons (car Xs) (cdr Xs)) = Xs, for non-null Xs.

% We can just write the spec up to produce the datatype!
% We simply transform /functions/ car and cdr into relations;
% leaving the constructor, cons, alone.

% What are lists?
list(nil).
list(cons(_, Xs)) :- list(Xs).

null(nil).

car(cons(X, Xs), X) :- list(Xs).
cdr(cons(_, Xs), Xs) :- list(Xs).

% ?- true.
% - list(Ys), not(null(L)), list(cons(car(Ys, Y), cdr(Ys, L))). % loops.

% ?- [1] = [1|[]].
%% ADT: Pairs, Numbers, Lists, and Trees:3 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Built-in%20Lists][Built-in Lists:1]]
% ?- ["one", two, 3] = [Head|Tail].
%% Built-in Lists:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Built-in%20Lists][Built-in Lists:2]]
member(element, list)
append(list1, list2, lists12)
prefix(part, whole)
nth0(index, list, element)
last(list, element)
length(list, number)
reverse(list1, list2)
permutation(list1, list2)
sum_list(list, number)
max_list(list, number)
is_set(list_maybe_no_duplicates)
%% Built-in Lists:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Higher-order][Higher-order:1]]
colour(bike, red).
colour(chair, blue).

% Crashes!
% is_red(C, X, Y) :- C(X, Y)

% Works
is_red(C, X, Y) :- call(C, X, Y).

% ?- is_red(colour, bike, X). %⇒ X = red.
%% Higher-order:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*~Print,%20var,%20nonvar~][~Print, var, nonvar~:1]]
% ?- var(Y).  %⇒ true
% ?- Y = 2, var(Y). %⇒ false
% ?- Y = 2, nonvar(Y). %⇒ true
%% ~Print, var, nonvar~:1 ends here
