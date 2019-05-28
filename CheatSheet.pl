%% [[file:~/PrologCheatSheet/CheatSheet.org::*Interactive%20Prolog%20Setup][Interactive Prolog Setup:4]]
magicNumber(7).
magicNumber(9).
magicNumber(42).

% ?- magicNumber(8).
% ?- magicNumber(X).
%% Interactive Prolog Setup:4 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Interactive%20Prolog%20Setup][Interactive Prolog Setup:5]]
main :- write('Hello, world!').

% ?- main.
%% Interactive Prolog Setup:5 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Nullary%20Facts][Nullary Facts:1]]
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
%% Nullary Facts:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Nullary%20Facts][Nullary Facts:2]]
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
%% Nullary Facts:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Lists][Lists:1]]
% ?- ["one", two, 3] = [Head|Tail].
% ?- ["one", two, 3] = [H|T|G].
%% Lists:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Basics][Basics:1]]
% ?- X = 3 + 2.  %% X = 3 + 2
% ?- X is 3 + 2. %% X = 5
%% Basics:1 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Basics][Basics:2]]
yum(pie).
yum(apples).
yum(maths).

% ?- yum(Y), writeln(Y), fail. %⇒ pie apples maths false.
%% Basics:2 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Basics][Basics:3]]
% ?- var(Y).  %⇒ true
% ?- Y = 2, var(Y). %⇒ false
% ?- Y = 2, novar(Y). %⇒ true
%% Basics:3 ends here
