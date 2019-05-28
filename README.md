<h1> PrologCheatSheet </h1>

Basics of relational programming with Prolog
&#x2014;PROgramming in LOGic
^\_^

[Try Prolog online](https://swish.swi-prolog.org/p/algorex_prolog.pl)

**The listing sheet, as PDF, can be found
[here](<https://github.com/alhassy/PrologCheatSheet/blob/master/CheatSheet.pdf>)**,
while below is an unruly html rendition.

This reference sheet is built around the system
<https://github.com/alhassy/CheatSheet>.


# Table of Contents

1.  [Homemade Interactive Prolog Setup](#orgb71f0cf)
2.  [Basics](#org9d0e92c)
3.  [Unification](#org9aa2994)
4.  [Facts &#x2014;Nullary Relations](#org2144e75)
5.  [Hidden Quantifiers](#orgb30c1e8)
6.  [Conjunction](#org85d9676)
7.  [Disjunction](#org7661afe)
8.  [Arithmetic with `is`](#orgc08e81f)
9.  [Declaration Ordering Matters](#org683ab5c)
10. [Cuts](#org0254154)
11. [ADT: Pairs, Numbers, Lists, and Trees](#org3d7cdca)
12. [Built-in Lists](#orgf99beb6)
13. [Higher-order](#org527cf77)
14. [`Print, var, nonvar`](#orgdabc9c4)
15. [Reads](#org41fddeb)














<a id="orgb71f0cf"></a>

# Homemade Interactive Prolog Setup

In Prolog, one declares a relationship `r(x0, x1, …, xn)` to be true for the declared
`xi` &#x2014;with a change of perspective any of the `xi` can be considered ‘input’ and the
rest considered ‘output’.

    (use-package prolog)

    ;; Obtain “swipl” interpreter.
    (async-shell-command "brew install swi-prolog")

    ;; alhassy-air:~ musa$ swipl --version
    ;; SWI-Prolog version 8.0.2 for x86_64-darwin

The following did not work for me :'( &#x2014;so I made my own lolz.

    (use-package ob-prolog)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((prolog . t)))

    (use-package ediprolog)

Here's my current setup:

    (local-set-key (kbd "<f6>") (lambda () (interactive)
      "
	org-babel-tangle the whole file, then execute the final query
	in the current SRC block.

	If the query mentions the variable ‘X’, then show all possible solutions
	followed by ‘false’. Usually one presses ‘;’ to see other solutions,
	but in Emacs this only shows one futher solution then terminates.
	We get around this by executing essentially
	“forall(your-query-with-X, writeln(X)).”
	This prints all solutions X to your query.

	If you want to use a variable but don't want to see all solutions,
	then avoid using ‘X’; e.g., use ‘Y’ ^_^.
      "
      (-let [kill-buffer-query-functions nil]
      (ignore-errors
	 (switch-to-buffer "*Prolog*")
	 (kill-buffer "*Prolog*"))

      ;; Get final query in current source block
      (search-forward "#+END_SRC")
      (search-backward "% ?-")
      ;; Copy line, without killing it.
      (setq xx (thing-at-point 'line t))

      (async-shell-command (format "swipl -s %s" (car (org-babel-tangle))) "*Prolog*")
      (other-window 1)

      ;; Paste the final query
      (setq xx (s-chop-prefix "% ?- " xx))
      (when (s-contains? "X" xx)
	(setq xx (concat "writeln(\"X =\"), forall(" (s-replace "." ", writeln(X))." xx))))

      (insert xx)
      (comint-send-input nil t) ;; Send it, i.e., “press enter at prompt”.

      ;; Insert query again, but do not send, in case user wishes to change it.
      (insert xx)
      (previous-line) (end-of-line)

    )))

For example:

    magicNumber(7).
    magicNumber(9).
    magicNumber(42).

    % ?- magicNumber(8).
    % ?- magicNumber(X).

Press `f6` to obtain all solutions `X` to this query :grin:

Or

    main :- write('Hello, world!').

    % ?- main.

This little setup has made exploring Prolog fun for me; hopefully it will make
Prolog fun for others 😄


<a id="org9d0e92c"></a>

# Basics

*Everything is a relation!* &#x2014;I.e., a table in a database!

\room
Whence programs are [unidirectional](https://blog.algorexhealth.com/2018/11/a-practo-theoretical-introduction-to-logic-programming/) and can be ‘run in reverse’:
Input arguments and output arguments are the same
thing! Only perspective shifts matter.

\room
For example, defining a relation `plus(X, Y, Sum)`
*intended* to be true precisely when `Sum ≈ X + Y`
gives us two other methods!
Subtract: `plus(4, Y, 8)` yields all solutions `Y` to
the problem `8 = 4 + Y`.
Partitions: `plus(X, Y, 8)` yields all pairs `X, Y`
that sum to 8.

Prolog is PROgramming in LOGic.

-   Prolog is declarative: A program is a collection of ‘axioms’ from which ‘theorems’
    can be proven. For example, consider how sorting is performed:

    -   Procedurally: Find the minimum in the remainder of the list, swap it with the head
	of the list; repeat on the tail of the list.

    -   Declaratively: `B` is the sorting of `A` *provided* it is a permutation of `A` and it is
	ordered.

    Whence, a program is a theory and computation is deduction!


<a id="org9aa2994"></a>

# Unification

-   **Unification:** Can the given terms be made to represent the same structure?
    -   This is how type inference is made to work in all (?) languages.
-   **Backtracking:** When a choice in unification causes it to fail, go back to the
    most recent choice point and select the next avialable choice.

\room
Unification:

1.  A constant unified only with itself.
2.  A variable unifies with anything.
3.  Two structures, terms, unify precisely when they have
    the same head and the same number of arguments,
    and the corresponding arguments unify recursively.

\room
Unification performs no simplification, whence no arithmetic.
This means, for example, we can form pairs by sticking an infix operator between two items; moreover we can form distinct kinds of pairs by using different operators:

    ?- C + "nice" = woah + Z.
    C = woah,
    Z = "nice".

    % ‘+’ and ‘/’ are different, so no way to make these equal.
    ?- C + "nice" = woah / Z.
    false.


<a id="org2144e75"></a>

# Facts &#x2014;Nullary Relations

We declare relations by having them begin with a lowercase letter;
variables are distinguished by starting with a capital letter.

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

Here's a cute one:

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


<a id="orgb30c1e8"></a>

# Hidden Quantifiers

    head(X) :- body(X,Y).
    % Semantics: ∀ X. head(X) ⇐ ∃ Y. body(X,Y).

Queries are treated as headless clauses.

    ?- Q(X)
    % Semantics: ∃ X. Q(X).


<a id="org85d9676"></a>

# Conjunction

-   Conjunction: `p(X), q(X)` means “let `X` be *a* solution to `p`, then use it in query `q`.”

-   Operational semantics: Let `X` be the first solution declared, found, for `p`,
    then try `q`; if it fails, then *backtrack* and pick the next declared solution to `p`,
    if any, and repeat until `q` succeeds.

-   For example, `p(X), print(X), fail.` gets a solution to `p`, prints it, then fails
    thereby necessitating a backtrack to obtain a different solution `X` for `p`, then
    repeats. In essence, this is prints all solutions to `p`
    &#x2014;a so-called “fail driven loop”.

For example,

    yum(pie).
    yum(apples).
    yum(maths).

    % ?- yum(Y), writeln(Y), fail. %⇒ pie apples maths false.


<a id="org7661afe"></a>

# Disjunction

Since a Prolog program is the conjunction of all its clauses:

    % (head ⇐ body₁) ∧ (head ⇐ body₂)
    head :- body₁.
    head :- body₂.

    ≅

    % head  ⇐  body₁ ∨ body₂
    head :- body₁ ; body₂.


<a id="orgc08e81f"></a>

# Arithmetic with `is`

-   Unification only tries to make both sides of an equality true by binding free
    variables to expressions. It does not do any arithmetic.

-   Use `is` to perform arithmetic.

    % ?- X = 3 + 2.  %% X = 3 + 2
    % ?- X is 3 + 2. %% X = 5


<a id="org683ab5c"></a>

# Declaration Ordering Matters

When forming a recursive relation, ensure the base case, the terminating portion,
  is declared before any portions that require recursion. Otherwise the program may
  loop forever.

\room
Unification is performed using depth-first search using the order of the declared
  relationships. For example, the following works:

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


<a id="org0254154"></a>

# Cuts

-   Ensure deterministic behaviour:
    Discard choice points of a ancestor frames.

-   `p(X, a), !` only produces one answer to `X`:
    Do not search for additional solutions once *a* solution has been found to `p`.

    E.g., only one `X` solves the problem and trying to
    find another leads to infinite search &#x2014;“green cut”&#x2014;
    or unintended candidate results &#x2014;“red cut”.


<a id="org3d7cdca"></a>

# ADT: Pairs, Numbers, Lists, and Trees

-   Uniform treatment of all datatypes as predicates!

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

    % In Haskell: Tree a = Leaf a | Branch (Tree a) (Tree a)

    tree(leaf(_)).
    tree(branch(L, R)) :- tree(L), tree(R).

    % ?- A = leaf(1), B = leaf(2), L = branch(A, B), R = branch(A, A), tree(branch(L, R)).

Programming via specification: Lisp lists, for example, are defined by the following
equations.

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


<a id="orgf99beb6"></a>

# Built-in Lists

Lists are enclosed in brackets, separated by commas,
and constructed out of cons “|”.

    % ?- ["one", two, 3] = [Head|Tail].

See [here](http://www.swi-prolog.org/pldoc/man?section=lists) for the list library, which includes:

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


<a id="org527cf77"></a>

# Higher-order

-   Prolog is limited to first-order logic: We cannot bind variables to relations.
-   Prolog *indirectly* supports higher-order rules.

    colour(bike, red).
    colour(chair, blue).

    % Crashes!
    % is_red(C, X, Y) :- C(X, Y)

    % Works
    is_red(C, X, Y) :- call(C, X, Y).

    % ?- is_red(colour, bike, X). %⇒ X = red.


<a id="orgdabc9c4"></a>

# `Print, var, nonvar`

`Print` predicate always succeeds, never binds any variables, and prints out its
parameter as a side effect.

\room
Use built-ins `var` and `nonvar` to check if a variable is free or bound.

    % ?- var(Y).  %⇒ true
    % ?- Y = 2, var(Y). %⇒ false
    % ?- Y = 2, nonvar(Y). %⇒ true

\newpage


<a id="org41fddeb"></a>

# Reads

Organised in terms of length:

-   [Introduction to logic programming with Prolog](https://www.matchilling.com/introduction-to-logic-programming-with-prolog/) &#x2014;12 minute read.
-   [Introduction to Prolog](http://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/index.html#menu) &#x2014;with interactive quizzes
-   [Derek Banas' Prolog Tutorial](https://www.youtube.com/watch?v=SykxWpFwMGs)  &#x2014;1 hour video
-   [A Practo-Theoretical Introduction to Logic Programming](https://blog.algorexhealth.com/2018/11/a-practo-theoretical-introduction-to-logic-programming/)  &#x2014;a **colourful** read showing Prolog ≅ SQL.
-   [Prolog Wikibook](https://en.wikibooks.org/wiki/Prolog) &#x2014;slow-paced and cute
-   [James Power's Prolog Tutorials](http://www.cs.nuim.ie/~jpower/Courses/Previous/PROLOG/)
-   [Introduction to Logic Programming](https://www3.risc.jku.at/education/courses/ws2009/logic-programming/) &#x2014;course notes and more!
-   [Stackoverflow Prolog Questions](https://stackoverflow.com/questions/tagged/prolog)  &#x2014;nifty FAQ stuff
-   [99 Prolog Problems](https://sites.google.com/site/prologsite/prolog-problems)   &#x2014;with solutions
-   [Backtracking](https://www.cis.upenn.edu/~matuszek/cit594-2012/Pages/backtracking.html)
-   [Escape from Zurg: An Exercise in Logic Programming](http://web.engr.oregonstate.edu/~erwig/papers/Zurg_JFP04.pdf)
-   [Use of Prolog for developing a new programming language](https://pdfs.semanticscholar.org/57d3/1ca47fa9688089b9b7e7c19c199aa03aff1e.pdf) &#x2014;Erlang!
-   [prolog :- tutorial](https://www.cpp.edu/~jrfisher/www/prolog_tutorial/pt_framer.html) &#x2014;Example oriented
-   [Learn Prolog Now!](http://www.learnprolognow.org/)  &#x2014;thorough, from basics to advanced
-   [Real World Programming in SWI-Prolog](http://www.pathwayslms.com/swipltuts/index.html)
