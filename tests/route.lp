/*
        $Id: route.lp,v 1.3 2000/01/14 10:29:34 sufrin Exp $
        
        Route planning

*/

flight(london,   paris,     45).
flight(paris,    berlin,    25).
flight(paris,    brussels,  90).
flight(berlin,   london,    60).
flight(berlin,   athens,    60).
flight(brussels, rome,      120).
flight(rome,     athens,    80).
flight(athens,   london,    150).
flight(dublin,   london,    150).

route(Start, Start, nil).
route(Start, Dest,  Next:Via) :-
        flight(Start, Next, Time),
        route(Next, Dest, Via).

route'(Start, Start, Via, Route) :- rev(Via, Route).
route'(Start, Dest,  Via, Route) :-
        flight(Start, Next, Time),
        not member(Next, Via),
        route'(Next, Dest, Next:Via, Route).
        
travel(Start, Dest, Route) :- route'(Start, Dest, Start:nil, Route).

city(X) :- flight(X, Y, T).
city(X) :- flight(Y, X, T).

/*
cities(XS, R)  :- city(X), not member(X, XS), cities(X:XS, R).
cities(XS, XS) :- .
*/

/*
        We have to use non-logical means to accumulate results, namely
        a failure-driven loop using a global assignment
*/

cities(R) :- fillCit, map_All(cit, R), map_Remove(cit).
fillCit   :- city(X), map_All(cit, R), not member(X, R), map_Add(cit, X), fail.
fillCit   :- .

/*
        Choice from a list: pattern is choose(List, Var), Var/=nil, process(Var), fail.
*/
choose(nil,  nil) :-.
choose(X:XS, X)   :- .
choose(X:XS, Y)   :- choose(XS, Y).



#use lists


