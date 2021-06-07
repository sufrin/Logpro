#use lib

#infix0 "-->"
#infix0 "-->*"
#infix1 "in"
#infix4 "=>"    /*      Abstraction    */
#infix4 "="     /*      Definition     */
#infix6 "+"
#infix6 "-"
#notfix "let"
#notfix "lambda"

constant(N)              :- num(N).
normalform(N)            :- num(N).
normalform({Bv => Body}) :- .


E + F --> V       :- constant(E), constant(F), sum(E, F, V).
E + F --> E' + F  :- E --> E'.
E + F --> E  + F' :- F --> F'.

{Bv => Body}[E]        --> Body'  :- subst(Bv, E, Body, Body').
F[E]                   --> F'[E]  :- F --> F'.
(let Bv = E in Body)   --> {Bv => Body}[E].

E -->* E   :- normalform(E), show(E).
E -->* E'' :- E --> E', show(E, (-->) ), E' -->* E''.

subst(Bv, E, Bv,  E)   :- atom(Bv).
subst(Bv, E, Bv', Bv') :- atom(Bv'), Bv'/=Bv.
subst(Bv, E, K,  K)    :- num(K).

subst(Bv, E, E1+E2, E1'+E2')   :- subst(Bv, E, E1, E1'), 
                                  subst(Bv, E, E2, E2').
                                  
subst(Bv, E, E1[E2], E1'[E2']) :- subst(Bv, E, E1, E1'), 
                                  subst(Bv, E, E2, E2').

subst(Bv, E, {Bv' => Body}, {Bv' => Body'}) :- Bv/=Bv', subst(Bv, E, Body, Body'). 
subst(Bv, E, {Bv  => Body}, {Bv  => Body }) :- . 

subst(Bv, E, let Bv'=E1 in E2, let Bv'=E1' in E2') :- subst(Bv, E, E1, E1'), 
                                                      subst(Bv, E, E2, E2'). 
  
subst(Bv, E, let Bv=E1 in E2, let Bv=E1' in E2') :- subst(Bv, E, E1, E1'), 
                                                    subst(Bv, E, E2, E2'). 
  
prob1 :- (let x=1 in x+x) -->* V.
prob2 :- (let x=1 in x+x)+(let y=4 in y+4) -->* V.
prob3 :- ({y=>{x=>x+y}}[x]) -->* V.

