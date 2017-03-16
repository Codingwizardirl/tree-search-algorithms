% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1525701
%
%
% ------------------------- Domain Definition -------------------------
% This file describes a planning domain: a set of predicates and
% fluents that describe the state of the system, a set of actions and
% the axioms related to them. More than one problem can use the same
% domain definition, and therefore include this file


% --- Cross-file definitions ------------------------------------------
% marks the predicates whose definition is spread across two or more
% files
%
:- multifile at/3, holding/2, arrived/2, adjacent/2.





% --- Primitive control actions ---------------------------------------
% this section defines the name and the number of parameters of the
% actions available to the planner
%
% primitive_action( dosomething(_,_) ).	% underscore means `anything'

primitive_action( move(_,_) ).	
primitive_action( pickUp(_) ).
primitive_action( drop(_,_) ).
primitive_action( transport(_) ).




% --- Precondition for primitive actions ------------------------------
% describe when an action can be carried out, in a generic situation S
%
% poss( doSomething(...), S ) :- preconditions(..., S).

poss(move(X,Y), S) :- adjacent(X,Y) , at(agent, X, S).

poss(pickUp(Object), S) :-
at(Object, Location, S), at(agent, Location, S),
not(Object = agent), not(arrived(Object, S)), not(holding(Object, S)).

poss(drop(Passenger, Location), S) :- holding(Passenger, S), at(agent, Location, S).

poss(transport(Passenger), S) :- at(agent,destination(Passenger), S), holding(Passenger, S).


% --- Successor state axioms ------------------------------------------
% describe the value of fluent based on the previous situation and the
% action chosen for the plan. 
%
% fluent(..., result(A,S)) :- positive; previous-state, not(negative)

at(Object, Location, result(A, S)) :-
	(A = move(_, Location), Object = agent); 
	(A = move(_, Location), holding(Object, S), Object = passenger); 
	(at(Object, Location, S), not(A = move(Location, _)));
	(at(Object, Location, S), not(holding(Object, S)), Object = passenger).


holding(Passenger, result(A, S)) :-
	(A = pickUp(Passenger));
	holding(Passenger, S), not(A = drop(Passenger,_)).

arrived(Passenger, result(A,S)) :-
  	(A = transport(Passenger)); arrived(Passenger, S).


% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
