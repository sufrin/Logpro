#use lists

intersect(XS, YS, X) :- member(X, XS), member(X, YS), !.
intersects(XS, YS, X) :- member(X, XS), member(X, YS) .

        choose(1) :-.
        choose(2) :-.
        choose(3) :-. 
        
        det(X)       :- choose(X), !.
        conj(X, Y)   :- choose(X), choose(Y).
        dconj(X, Y)  :- choose(X), !, choose(Y).
        ddconj(X, Y) :- choose(X), choose(Y), !.
        
        below(X)     :- !, choose(X).
        below(4)     :- .
        nbelow(X)     :- choose(X).
        nbelow(4)     :- .
/*
        intersect(1:2:3:4:nil, 2:3:4:nil, X)?
        intersects(1:2:3:4:nil, 2:3:4:nil, X)?
*/

merge(X:XS, Y:YS, X:ZS)   :- X<Y, !, merge(XS,   Y:YS, ZS).
merge(X:XS, Y:YS, Y:ZS)   :- X>Y, !, merge(X:XS, YS,   ZS).
merge(X:XS, Y:YS, X:Y:ZS) :- X=Y, !, merge(XS,   YS,   ZS).
merge(XS,   nil, XS)      :- !.
merge(nil,  YS,   YS)     :- !.

        




