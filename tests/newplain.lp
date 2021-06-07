/*
        Structural semantics of Plain
        
        
*/
#use "lib"
#infix6 "|->"
#infix5 "&"

lookup(X, U|->V&ENV,  V)      :- X==U.
lookup(X, U|->V&ENV,  R)      :- X/=U, 
                                 lookup(X, ENV, R).
/*lookup(X, {}, undef).*/

update(U|->V&ENV,  X|->Y, X|->Y&ENV)   :- X==U. 
update(U|->V&ENV,  X|->Y, U|->V&ENV')  :- X/=U, 
                                          update(ENV, X|->Y, ENV').
update({}, X|->Y, X|->Y & {}) :- .


#infix0 "-|>"
#infix0 "-|>*"
#infix3 ";"
#infix4 ":="
#infix7 "+"
#infix7 "-"

#notfix rule
rule(S) :- show(" (by", S, ")").

/*
        Single-step execution rules
*/

[|  I       |] S        -|>    [|  V     |] S   :- id(I),  
                                                   lookup(I, S, V),
                                                   rule "var".
                                                  
[|  I:=E    |] S        -|>    [|  skip  |] S'  :- id(I), 
                                                   con(E), 
                                                   update(S, I|->E, S'),
                                                   rule "assign.2".


[|  I:=E    |] S        -|>    [|  I:=E' |] S   :- not(con(E)), 
                                                   [| E |] S -|> [| E' |] S,
                                                   rule "assign.1".

                                                  
[|  P; Q    |] S        -|>    [|  P'; Q |] S'  :- P/=skip,
                                                   [| P |] S -|> [| P' |] S',
                                                   rule "seq.1".

[|  skip; Q |] S        -|>    [|  Q     |] S   :- rule "seq.2".

--      Prescribes left-to-right order

[|  E+F     |] S        -|>    [|  V     |] S   :- con(E), con(F),
                                                   sum(E, F, V),
                                                   rule "+2".

[|  E+F     |] S        -|>    [|  E'+F  |] S   :- not con(E),
                                                   [| E |] S -|> [| E' |] S,
                                                   rule "+1L".


[|  E+F     |] S        -|>    [|  E+F'  |] S   :- not con(F),
                                                   [| F |] S -|> [| F' |] S,
                                                   rule "+1R".


/*
        Run a program step-by-step
*/

CONFIG  -|>* CONFIG    :- normal(CONFIG),
                          show(CONFIG, "  finished.").           

CONFIG  -|>* CONFIG''  :- not(normal(CONFIG)),
                          show(CONFIG, "  -|>"),
                          CONFIG  -|> CONFIG',
                          CONFIG' -|>* CONFIG''.


/*      
        Auxiliary definitions

*/

id(I)                :- atom(I), I/=skip.
con(E)               :- num(E).
normal([| skip |] S) :- .
#notfix run
run P                :- [| P |] {} -|>* [| P' |] S'.

/*
        A few sample programs
*/
prob1  :- run x:=3.
prob2  :- run x:=3; y:=x.
prob2a :- run x:=3; y:=z.
prob3  :- run x:=3; y:=4; z:=x.
prob4  :- run x:=3; y:=4; z:=x+y.








