/*
        $Id: parse.lp,v 1.3 2000/01/12 10:24:00 sufrin Exp $
        
        Logic Parsing (Exponential version)
*/
#use lib
expr(T, In, Rest)   :- term(T1, In, In'), expr'(T1, T, In', Rest).

expr'(T, T, In, In) :-.

expr'(T1, T, In, Rest) :- eat("+", In, In'), 
                          term(T2, In', In''),
                          expr'(add(T1, T2), T, In'', Rest).

expr'(T1, T, In, Rest) :- eat("-", In, In'), 
                          term(T2, In', In''),
                          expr'(subtract(T1, T2), T, In'', Rest).
                          
term(T, In, Rest)   :- factor(T1, In, In'), term'(T1, T, In', Rest).
term'(T, T, In, In) :-.
term'(T1, T, In, Rest) :- eat("*", In, In'), 
                          factor(T2, In', In''),
                          term'(mul(T1, T2), T, In'', Rest).

term'(T1, T, In, Rest) :- eat("/", In, In'), 
                          factor(T2, In', In''),
                          term'(div(T1, T2), T, In'', Rest).

factor(vbl(Letter), In, Rest) :- eat1(Letter, In, Rest).

factor(T, In, Rest) :- eat("(", In, In'), 
                       expr(T, In', In''), 
                       eat(")", In'', Rest).


eat (Letter, In, Rest) :- spaces(In, In'), cat(Letter, Rest, In').
eat1(Letter, In, Rest) :- spaces(In, In'), hd(In', Letter), tl(In', Rest).

spaces(In, Rest) :- cat(" ", In', In), spaces(In', Rest).
spaces(In, In)   :-.



prob1(T) :- expr(T, "a +b *c +d*e+f", "").
        
