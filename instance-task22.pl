% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1525701
%
%
% ------------------------- Problem Instance --------------------------
% This file is a template for a problem instance: the definition of an
% initial state and of a goal. 

% debug(on).	% need additional debug information at runtime?



% --- Load domain definitions from an external file -------------------

:- ['domain-task1.pl'].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------
adjacent(a,b).
adjacent(b,a).
adjacent(b,c).
adjacent(c,b).
adjacent(g,h).
adjacent(h,g).
adjacent(h,d).
adjacent(d,h).
adjacent(e,f).
adjacent(f,e).
adjacent(a,g).
adjacent(d,b).
adjacent(h,f).
adjacent(f,c).

at(agent, a, s0).
at(passenger, a, s0).
destination(h).

% --- Goal condition that the planner will try to reach ---------------

goal(S) :- at(passenger, h, S).				% fill in the goal definition

% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
