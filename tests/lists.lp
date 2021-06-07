/*
        Lists
        
        $Id: lists.lp,v 1.8 2000/01/21 01:02:22 sufrin Exp $
*/
#use lib
#infix4 ":"

append(nil,  YS, YS)   :- .
append(X:XS, YS, X:ZS) :- append(XS, YS, ZS) .

rev2(nil,  R, R)   :- .
rev2(X:XS, R, ANS) :- rev2(XS, X:R, ANS).
rev(XS, YS) :- rev2(XS, nil, YS).

concat(nil,  nil) :- .
concat(X:XS, R)   :- concat(XS, R'), append(X, R', R).

member(X, Xs) :- append(Ls, X:Rs, Xs).

aug(Y, XS, XS)   :- member(Y, XS).
aug(Y, XS, Y:XS) :- not(member(Y, XS)).

nodups(XS, YS)       :- nodups(XS, nil, YS).
nodups(nil, YS, YS)  :- .
nodups(X:XS, YS, RS) :- if (member(X, YS), 
                            nodups(XS, YS, RS),
                            nodups(XS, X:YS, RS)).

inboth(X, Xs, Ys) :- member(X, Xs), member(X, Ys).

if(P, L, R) :- call(P), !, call(L).
if(P, L, R) :- call(R).

nd(XS,   RS)      :- nd(XS, nil, RS).
nd(nil,  YS, YS)  :- .
nd(X:XS, YS, RS)  :- member(X, YS), !, nd(XS, YS, RS).
nd(X:XS, YS, RS)  :- nd(XS, X:YS, RS).

