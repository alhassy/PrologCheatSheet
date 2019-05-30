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

% ?- X is 6 / 3. %̄⇒ X = 2.

% ?- X is  5 / 3. %̄⇒ X = 1.6666666666666667.

% ?- X is 5 // 3. %̄⇒ X = 1.

% ?- X is 5 mod 3. %̄⇒ X = 2.
%% Arithmetic with ~is~:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Arithmetic%20with%20~is~][Arithmetic with ~is~:2]]
% ?- name(woah_hello, X). %⇒ X = [119,111,97,104,95,104,101,108,108,111]
% ?- name(woah, X).       %⇒ X = [119,111,97,104]
%% Arithmetic with ~is~:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Arithmetic%20with%20~is~][Arithmetic with ~is~:3]]
% ?- atom_chars(nice, X). %⇒ X = [n, i, c, e].
%% Arithmetic with ~is~:3 ends here

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

%% [[file:~/PrologCheatSheet/CheatSheet.org::*The%20Cut][The Cut:1]]
a(X,Y) :- b(X), !, c(Y).
b(1). b(2). b(3).
c(1). c(2). c(3).

% ?- a(X, Y). %⇒ X = 1 ∧ Y = 1, X = 1 ∧ Y = 2, X = 1 ∧ Y = 3
%% The Cut:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*The%20Cut][The Cut:2]]
d(X) :- e(X), !, f(X).
e(1). e(2). e(3). f(2).

% ?- not(d(X)). %⇒ “no solution” since only e(1) considered.
% ?- d(2). %⇒ true, since no searching performed and 2 ∈ e ∩ f.

d_no_cut(X) :- e(X), f(X).
% ?- d_no_cut(X). %⇒ X = 2.
%% The Cut:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*The%20Cut][The Cut:3]]
g(X) :- h(X), !, i(X).
g(X) :- j(X).

h(1). h(4). i(3). j(4).

% ?- g(X).
%% The Cut:3 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*The%20Cut][The Cut:5]]
sum_to(0, 0) :- !.
sum_to(N, Res) :- M is N - 1, sum_to(M, ResM), Res is ResM + N.

% ?- sum_to(1, X).
%% The Cut:5 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*The%20Cut][The Cut:6]]
sum_to_not(0, 0).
sum_to_not(N, Res) :- N \= 0, M is N - 1, sum_to(M, ResM), Res is ResM + N.

% ?- sum_to_not(5, X). %⇒ X = 15.
%% The Cut:6 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*ADT:%20Pairs,%20Numbers,%20Lists,%20and%20Trees][ADT: Pairs, Numbers, Lists, and Trees:1]]
% In Haskell: Person = Me | You | Them
person(me).
person(you).
person(them).

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
% ?- ["one", two, 3] = [Head|Tail]. %⇒ Head = "one", Tail = [two, 3].
% ?- ["one", two, 3] = [_,Second|_]. %⇒ Second = two.
% ?- [[the, Y], Z] = [[X, hare], [is, here]]. %⇒ X = the, Y = hare, Z = [is, here]
%% Built-in Lists:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Built-in%20Lists][Built-in Lists:2]]
% Searching: x ∈ l?
elem(Item, [Item|Tail]). % Yes, it's at the front.
elem(Item, [_|Tail]) :- elem(Item, Tail). % Yes, it's in the tail.

% ?- elem(one, [this, "is", one, thing]). %⇒ true
% ?- elem(onE, [this, "is", one, thing]). %⇒ false
%% Built-in Lists:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Built-in%20Lists][Built-in Lists:4]]
% member is above, ‘elem’.

append([], List2, List2).
append([H|T], List2, [H|Append]) :- append(T, List2, Append).

% ?- append([1,"two", three], [four, "5", "6"], Result).

prefix([], List).
prefix([H|T], [H|List]) :- prefix(T, List).

% ?- prefix([1,2,three], [1, 2, three, four]).
% ?- not(prefix([1,2,three], [1, 2])).

nth0(0, [H|T], H).
nth0(X, [_|T], E) :- Y is X - 1, nth0(Y, T, E).

% ?- nth0(2, [one, two, three], X).
% ?- not(nth0(2, [one, two], X)).

last([H],H).
last([_|T], L) :- last(T, L).

% ?- last([1,2,3], L).
% ?- last([1,2], L).
% ?- not(last([], L)).

mylength([], 0).
mylength([H|T], Res) :- length(T, L), Res is L + 1.

% ?- mylength([1,2,3], L).

% count(E, L, N)  ≡  E occurs N times in L
count(E, [], 0).
count(E, [E|T], Res) :- count(E, T, N), Res is N + 1.
count(E, [_|T], N)   :- count(E, T, N).

% ?- count(2, [1,2,1,3,2], N).

% For each element x of list1, let n1 and n2 be the number of times x occurs in list1 and list2; they're bag-equal if n1 = n2. Note: elem requires a non-empty list.
%% Built-in Lists:4 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Using%20Modules][Using Modules:1]]
use_module(library(clpfd)).

% ?- all_distinct([1,"two", two]).
%% Using Modules:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Higher-order][Higher-order:1]]
colour(bike, red).
colour(chair, blue).

% Crashes!
% is_red(C, X, Y) :- C(X, Y)

% Works
is_red(C, X, Y) :- call(C, X, Y).

% ?- is_red(colour, bike, X). %⇒ X = red.
%% Higher-order:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Higher-order][Higher-order:2]]
% ?- p(a, b, c) =.. Y. %⇒ Y = [p, a, b, c].
% ?- Y =.. [p, a, b, c]. %⇒ Y = p(a, b, c).
%% Higher-order:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*~Print,%20var,%20nonvar~][~Print, var, nonvar~:1]]
% ?- var(Y).  %⇒ true
% ?- Y = 2, var(Y). %⇒ false
% ?- Y = 2, nonvar(Y). %⇒ true
%% ~Print, var, nonvar~:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*~Print,%20var,%20nonvar~][~Print, var, nonvar~:2]]
% ?- arg(2, foo(x, y), y). %⇒ true
%% ~Print, var, nonvar~:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Meta-Programming][Meta-Programming:1]]
test(you, me, us).
test(A, B, C) :- [A, B, C] = [the, second, clause].

% ?- clause(test(Arg1, Arg2, Arg3), Body).
% ⇒ ‘Body’ as well as ‘Arg𝒾’ are unified for each clause of ‘test’.
%% Meta-Programming:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Meta-Programming][Meta-Programming:2]]
% interpret(G) succeeds as a goal exactly when G succeeds as a goal.

% Goals is already true.
interpret(true) :- !.

% A pair of goals.
interpret((G, H)) :- !, interpret(G), interpret(H).

% Simple goals: Find a clause whose head matches the goal and interpret its subgoals.
interpret(Goal) :- clause(Goal,Subgoals), interpret(Subgoals).

% ?- interpret(test(A, B, C)).
%% Meta-Programming:2 ends here
