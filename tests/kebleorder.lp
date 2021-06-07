        walk(n, stgiles, balliol,   oucs)   :- .
        walk(e, keble,   oucs,      oucl)   :- .
        walk(n, parks,   oucl,      eng)    :- .
        walk(s, banbury, eng,       oucs)   :- .
        walk(s, parks,   oucl,      museum) :- .
        
        -- walk(n, tunnel,  museum,    oucs)   :- .
        
        go(A, B) :- walk(Dir, Rd, A, B).
        -- go(B, A) :- walk(Dir, Rd, A, B).
        
        access(From, From) :- .
        access(From, To)   :- access(Place, To), go(From, Place).

