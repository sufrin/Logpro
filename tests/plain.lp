/*
        Structural semantics of Plain
        
        
*/
#use    map
#infix0 "-|>"
#infix0 "-|>*"
#infix1 "|"
#infix3 ";"
#infix4 ":="
#infix7 "+"
#infix7 "-"

/*
        Single-step execution rules
*/

I       | S        -|>             V | S       :- id(I),  
                                                  lookup(I, S, V).
                                                  
I:=E    | S        -|>          skip | S'      :- id(I), 
                                                  con(E), 
                                                  update(S, I|->E, S').

I:=E    | S        -|>         I:=E' | S       :- not(con(E)), 
                                                  E|S -|> E'|S.
                                                  
   P; Q | S        -|>         P'; Q | S'      :- P/=skip,
                                                  P|S -|> P'|S'.

skip; Q | S        -|>             Q | S       :- .

E+F     | S        -|>          E'+F | S       :- not(con(E)),
                                                  E|S -|> E'|S.

E+F     | S        -|>          E+F' | S       :- con(E), not(con(F)),
                                                  F|S -|> F'|S.

E+F     | S        -|>             V | S       :- con(E), con(F),
                                                  sum(E, F, V).


/*
        Run a program step-by-step
*/

CONFIG  -|>* CONFIG    :- normal(CONFIG),
                          show(CONFIG, "  finished.").           

CONFIG  -|>* CONFIG''  :- not(normal(CONFIG)), 
                          show(CONFIG, " -|> "),
                          CONFIG  -|> CONFIG',
                          CONFIG' -|>* CONFIG''.

/*      
        Auxiliary definitions

*/

id(I)            :- atom(I), I/=skip.
con(E)           :- num(E).
normal(skip | S) :- .
run(P)           :- P | {} -|>* P' | S'.

/*
        A few sample programs
*/
prob1 :- run(x:=3).
prob2 :- run(x:=3; y:=4).
prob3 :- run(x:=3; y:=4; z:=x).
prob4 :- run(x:=3; y:=4; z:=x+y).





