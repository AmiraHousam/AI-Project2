:- use_module(library(lists)).
:- consult('KB.pl').


removeShip(X,Y,[[X,Y]|T],T).

removeShip(X,Y,[[A,B]|T1],[[A,B]|T2]):-
    removeShip(X,Y,T1,T2).

canPickup(X,Y,RemainingPassengers,R):-
    member([X,Y],RemainingPassengers),
	removeShip(X,Y,RemainingPassengers,R).

state(M,N,X,Y,Ships,0,s0):-
	grid(M,N),
    agent_loc(X,Y),
    ships_loc(Ships).


state(M,N,X,Y,Ships,C,R) :-
    state(M,N,X1,Y1,NewShips,C1,R1),
    (
	(X is X1 - 1,
    	X >= 0,
    	Y1 = Y,
    	C1 = C,
    	NewShips = Ships,
    	R = result(up,R1));

	(X is X1 + 1,
    	X < M,
    	Y1 = Y,
    	C1 = C,
    	NewShips = Ships,
    	R = result(down,R1));

	(Y is Y1 - 1,
    	Y >= 0,
    	X1 = X,
    	C1 = C,
    	NewShips = Ships,
    	R = result(left,R1));
	
	(Y is Y1 + 1,
    	Y < N,
    	X = X1,
    	C1 = C,
    	NewShips = Ships,
    	R = result(right,R1));
		
    (canPickup(X1,Y1,NewShips,Ships),
    	Y = Y1,
    	X = X1,
    	C is C1 + 1,
    	capacity(MaxCapacity),
    	C1 < MaxCapacity,
    	R = result(pickup,R1));
		
    (station(X1,Y1),
    	C1 > 0,
    	C = 0,
    	X = X1,
    	Y = Y1,
    	Ships = NewShips, 
    	R = result(drop,R1))
	).

goal(S):-
    ships_loc([]),
    state(M,N,X,Y,[],0,S),
    grid(M,N),
    station(X,Y).
	
goal(S):-
    S = result(drop,_),
    state(M,N,X,Y,[],0,S),
	grid(M,N),
    station(X,Y).