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
#infix2 "after"

#notfix rule
rule(S) :- show(" (by", S, ")").

/*
        Single-step execution rules
*/

I       | S        -|>             V | S       :- id(I),  
                                                  lookup(I, S, V),
                                                  rule id.
                                                  
I:=E    | S        -|>          skip | S'      :- id(I), 
                                                  con(E), 
                                                  update(S, I|->E, S'),
                                                  rule "ass.2".

I:=E    | S        -|>         I:=E' | S'      :- not(con(E)), 
                                                  E|S -|> E'|S',
                                                  rule "ass.1".
                                                  
   P; Q | S        -|>         P'; Q | S'      :- P/=skip,
                                                  P|S -|> P'|S',
                                                  rule "seq.1".

skip; Q | S        -|>             Q | S       :- rule "seq.2".

E+F     | S        -|>          E'+F | S'      :- not(con(E)),
                                                  E|S -|> E'|S',
                                                  rule "+1L".

E+F     | S        -|>          E+F' | S'      :- not(con(F)),
                                                  F|S -|> F'|S',
                                                  rule "+1R".

E+F     | S        -|>             V | S       :- con(E), con(F),
                                                  sum(E, F, V),
                                                  rule "+2".

E after skip | S   -|>             E | S       :- rule "after.2".

E after P    | S   -|>    E after P' | S'      :- P/=skip,
                                                  P|S -|> P'|S',
                                                  rule "after.1".

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
prob5 :- run(x:=3; y:=4; z:=(x after x:=x+1)).




