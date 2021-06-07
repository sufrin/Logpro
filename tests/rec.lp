/*
        
 $Id: rec.lp,v 1.1 1999/12/01 00:25:34 sufrin Exp $       
        
*/

add(0,    N, N)    :- .
add(s(M), N, s(R)) :- add(M, N, R).

i2n(0, 0)    :- .
i2n(N, s(R)) :- pred(N, M), i2n(M, R).

pred(N, R) :- succ(R, N).
fac (0, 1) :- .
fac (N, R) :- pred(N, M), fac(M, FM), prod(N, FM, R).



