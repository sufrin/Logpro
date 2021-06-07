/*
        $Id: quadparse.lp,v 1.1 2000/01/05 22:29:09 sufrin Exp $
        
        Logic Parsing (Exponential version)
*/
#use lib
expr(T, In, Rest) :- term(T, In, Rest).

expr(add(T1, T2), In, Rest) :-
    term(T1, In, In'), eat("+", In', In''), expr(T2, In'', Rest).

term(T, In, Rest) :- factor(T, In, Rest).
term(mul(T1, T2), In, Rest) :-
    factor(T1, In, In'), eat("*", In', In''), term(T2, In'', Rest).
term(div(T1, T2), In, Rest) :-
    factor(T1, In, In'), eat("/", In', In''), term(T2, In'', Rest).

factor(var(Letter), In, Rest) :- eat(Letter, In, Rest).
factor(T, In, Rest) :- eat("(", In, In'), 
                       expr(T, In', In''), 
                       eat(")", In'', Rest).

eat(L, M, N) :- hd(M, L), tl(M, N).

prob1(T) :- expr(T, "a+b*c+d*e+f", U).
        
