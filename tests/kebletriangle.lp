/*
        $Id: kebletriangle.lp,v 1.3 2000/01/17 01:06:37 sufrin Exp $
        
        Route planning

*/
walk(n, stgiles, balliol,   oucs).
walk(e, keble,   oucs,      oucl).
walk(n, parks,   oucl,      eng).
walk(s, banbury, eng,       oucs).
walk(s, parks,   oucl,      museum).

go(A, B) :- show(go(A,B)),
              walk(Dir, Rd, A, B) 
            , show(go''(A,B))
            .
-- go(B, A) :- walk(Dir, Rd, A, B).


access(From, From) :- .
access(From, To) :- go(From, Place), access(Place, To).

access'(N, From, From) :- show(access'(N, From, From))
                       .
access'(N, From, To)   :- show (access'(N, From, To)), succ(N, N'),
                          access'(N', Place, To), 
                          go(From, Place)
                          , show (access''(N, From, To))
                          .

#count on

route(From, From, Via, Route) :- rev(Via, Route).
route(From, To,   Via, Route) :-
        go(From, Next), 
        not member(Next, Via),
        route(Next, To, Next:Via, Route).

route'(From, From, Via, Route) :- rev(Via, Route).
route'(From, To,   Via, Route) :-
        go(From, Next), 
        route'(Next, To, Next:Via, Route),
        not member(Next, Via)
        .
        
route(From, To, Route)  :- route(From, To, From:nil, Route).
route'(From, To, Route) :- route'(From, To, From:nil, Route).
        
        member(X, XS) :- append(L, X:R, XS).
        rev3(nil,  YS, YS)      :- .                     
        rev3(X:XS, YS, ANSWER)  :- rev3(XS, X:YS, ANSWER).  
        rev(In, Out) :- rev3(In, nil, Out).
        append(nil, YS, YS) :- .
        append(X:XS, YS, X:ZS) :- append(XS, YS, ZS).
        
        


