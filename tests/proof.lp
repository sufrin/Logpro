/*
        $Id: proof.lp,v 1.1 1999/12/01 18:00:59 sufrin Exp $
        
*/

#infix5 "=="
#infix5 "/="
X==X :- .
X/=Y :- not(X==Y).

#infix1 ";"
member(H, X;XS) :- H==X.
member(H, X;XS) :- H/=X, member(H, XS).

#infix0 "|-"
#infix4 "/\"
#infix3 "\/"
#infix2 "->"

proven(HS; A/\B |- C) :- proven(HS;A;B |- C).
proven(A/\B |- C)     :- proven(A;B |- C).


proven(HS; H |- H)    :- .

proven(HS |- A/\B)    :- proven(HS |- A), proven(HS |- B).



