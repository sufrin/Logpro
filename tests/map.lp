/*
        Finite mappings
        
        $Id: map.lp,v 1.6 2000/02/07 18:04:08 sufrin Exp $
        
*/
#use    lib
#infix6 "|->"
#infix5 "&"

lookup(X, U|->V&ENV,  V)      :- X==U.
lookup(X, U|->V,      V)      :- X==U.
lookup(X, U|->V,      undef)  :- X/=U.
lookup(X, U|->V&ENV,  R)      :- X/=U, 
                                 lookup(X, ENV, R).
lookup(X, {}, undef).

update(U|->V&ENV,  X|->Y, X|->Y&ENV)   :- X==U. 
update(U|->V,      X|->Y, X|->Y)       :- X==U. 
update(U|->V,      X|->Y, U|->V&X|->Y) :- X/=U. 
update(U|->V&ENV,  X|->Y, U|->V&ENV1)  :- X/=U, 
                                          update(ENV, X|->Y, ENV1).
update({}, X|->Y, X|->Y) :- .

extend(ENV, BINDING, BINDING&ENV) :-.




