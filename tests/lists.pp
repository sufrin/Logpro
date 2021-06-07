append(X:XS, YS, X:ZS) :- append(XS, YS, ZS) .
append(nil,  YS, YS)   :- .
member(X, Xs) :- append(Ls, X:Rs, Xs).
nodups(XS,   RS)  :- nd(XS, nil, RS).
nd(nil,  YS, YS)  :- .
nd(X:XS, YS, RS)  :- member(X, YS), !, nd(XS, YS, RS).
nd(X:XS, YS, RS)  :- nd(XS, X:YS, RS).

/*
        nodups(1:1:1:2:3:1:4:nil, YS)?
        nodups(1:1:1:1:2:3:1:4:nil, YS)?
        nodups(1:1:1:1:1:2:3:1:4:nil, YS)?
        nodups(1:2:1:3:1:1:1:1:1:1:4:nil, YS)?
*/


