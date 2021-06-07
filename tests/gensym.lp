/*
        $Id: gensym.lp,v 1.1 2000/01/03 01:09:31 sufrin Exp $
        
        Symbol generator using the inbuilt updateable dictionary to
        store the current symbol counter. This is dirty, but not half
        as dirty as using asserta, assertz, ....
*/

map_Lookup_default(K, V, D) :- map_Lookup(K, V), !.
map_Lookup_default(K, V, V) :- .

gensym(ROOT, R) :- map_Lookup_default(gensymcount, N, 0), 
                   succ(N, N'), 
                   map_Enter(gensymcount, N'), 
                   toString(N', COUNTER),
                   cat(ROOT, COUNTER, SYMBOL),
                   toFormula(SYMBOL, R).

gensym(R) :- gensym("gensym", R).

#use type

fixtype(T, U)         :- var(T), gensym("t", U), !.
fixtype(T->T', U->U') :- fixtype(T, U), fixtype(T', U'), !.
fixtype(T, T)         :- nonvar(T), !.
fix(T) :- fixtype(T, T).


             
