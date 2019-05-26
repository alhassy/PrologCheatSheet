%% [[file:~/PrologCheatSheet/CheatSheet.org::*Interactive%20Prolog%20Setup][Interactive Prolog Setup:4]]
magicNumber(7).
magicNumber(9).
magicNumber(42).

% ?- magicNumber(8).
% ?- magicNumber(X).
%% Interactive Prolog Setup:4 ends here

%% [[file:~/PrologCheatSheet/CheatSheet.org::*Interactive%20Prolog%20Setup][Interactive Prolog Setup:5]]
main :-
	write('Hello, world!').

% ?- main.
%% Interactive Prolog Setup:5 ends here

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
