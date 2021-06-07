#use lookup
#use lib

#infix0 "::"
#infix1 "in"
#infix4 "=>"    /*      Abstraction    */
#infix4 "="     /*      Definition     */
#infix6 "+"
#infix6 "-"
#notfix "let"
#notfix "lambda"

eval(Env, Num, Num)   :- num(Num), 
                         rshow(Env, Num, Num).

eval(Env, Var, Val)   :- atom(Var), lookup(Var, Env, Val),
                         rshow(Env, Var, Val).
                         
eval(Env, T1+T2, Val) :- pshow(Env, T1+T2, Val),
                         eval(Env, T1, V1), 
                         eval(Env, T2, V2), 
                         sum(V1, V2, Val),
                         rshow(Env, T1+T2, Val).

eval(Env, T1&T2, V1&V2) :- pshow(Env, T1&T2, Val),
                           eval(Env, T1, V1), 
                           eval(Env, T2, V2), 
                           rshow(Env, T1&T2, V1&V2).
                         
eval(Env, Fun[Term], Val') :-
                        pshow(Env, Fun[Term], Val'),
                        eval(Env, Fun, closure(Var, Term', Env')),
                        eval(Env, Term, Val),
                        extend(Var|->Val, Env', Env''),
                        eval(Env'', Term', Val'),
                        rshow(Env, Fun[Term], Val').
                        
eval(Env, lambda Bv => Body, closure(Bv, Body, Env)).

eval(Env, let Var=Term in Term', Val') :-
                         pshow(Env, let Var=Term in Term', Val'),
                         eval(Env, Term, Val),
                         extend(Var|->Val, Env, Env'),
                         eval(Env', Term', Val'),
                         rshow(Env, let Var=Term in Term', Val').
                         
prob0(V) :- eval (nil, let a=3 in a+a, V).
prob1(V) :- eval (nil, let a=3 in a&a&a, V).
prob2(V) :- eval (nil, let a=3 in let f=(lambda x => x+a) in f[4], V).

pshow(Env, Term, Val) :- show(evalE(Env, Term, Val)).
rshow(Env, Term, Val) :- show(evalR(Env, Term, Val)).


