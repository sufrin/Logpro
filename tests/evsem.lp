/*
        Evaluation Semantics
        
        $Id: evsem.lp,v 1.1 1999/12/01 00:25:34 sufrin Exp $
*/
#use map

#infix0 ";"
#infix1 ":="
#infix4 "+"
#infix4 "-"

eval(S,  N,    N,  S)  :- num(N).
eval(S,  N,    R,  S)  :- atom(N), lookup(N, S, R).
eval(S0, M+N,  R,  S2) :- eval(S0, M, EM, S1), eval(S1, N, EN, S2), sum(EM, EN, R).
eval(S0, M-N,  R,  S2) :- eval(S0, M, EM, S1), eval(S1, N, EN, S2), diff(EM, EN, R).
eval(S0, M;N,  EN, S2) :- eval(S0, M, EM, S1), eval(S1, N, EN, S2).
eval(S0, X:=E, EM, S2) :- atom(X), eval(S0, E, EM, S1), update(S1, X|->EM, S2).

testeval(V, S) :-
        eval(empty,
             x:=3;
             y:=4;
             z:=(x:=x+1)+(x:=y+1), 
             V, S).

