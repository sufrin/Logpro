/*

        Type system for a simple language

*/
#use lookup

#infix0 "::"
#infix1 "in"
#infix4 "=>"    /*      Abstraction    */
#infix4 "="     /*      Definition     */
#infix3 ";"     /*      Extension      */
#infix3 "|"     /*      Disjunction    */
#infix2 "->"    /*      Function arrow */
#infix3 "><"    /*      Product        */
#infix6 "+"
#infix6 "-"
#infix5 "^"     /*      Pair           */
#notfix "let"

type(ENV, N,      int)       :- num(N),  rshow(ENV, N, int).
type(ENV, N,      T)         :- atom(N), lookup(N, ENV, T), rshow(ENV, N, T).
type(ENV, M+N,    int)       :- type(ENV, M, int), type(ENV, N, int).
type(ENV, M-N,    int)       :- type(ENV, M, int), type(ENV, N, int).
type(ENV, M^N,    Tm><Tn)    :- type(ENV, M, Tm),  type(ENV, N, Tn).

type(ENV, V=>B,   T->T')     :- declpat(ENV, V|->T, ENV'),
                                type(ENV', B, T'),
                                rshow(ENV, V=>B, T->T') .

type(ENV, V=>B|F, T->T')     :- declpat(ENV, V|->T, ENV'),
                                type(ENV', B, T'),
                                type(ENV,  F, T->T').

type(ENV, F[A],     T')       :- type(ENV, F, T->T'), 
                                 type(ENV, A, T),
                                 rshow(ENV, F[A], T').
                                
type(ENV, let F[X]=E in B, T')   :-  !,
                                     type   (ENV,   X=>E,  T),
                                     declpat(ENV,   F|->T, ENV''),
                                     type   (ENV'', B,     T'),
                                     rshow  (ENV,   let(F[X]=E in B), T').
                                
type(ENV, let V=E in B, T')   :-  type   (ENV,   E,     T),
                                  declpat(ENV,   V|->T, ENV''),
                                  type   (ENV'', B,     T'),
                                  rshow  (ENV,   let(V=E in B), T').

type(ENV, let (V=E);D in B, T') :-  type   (ENV,    E,        T),
                                    declpat(ENV,    V|->T,    ENV''),
                                    type   (ENV'',  let(D in B), T'),
                                    rshow  (ENV,    let((V=E);D in B), T').
                                 
declpat(ENV, V|->T, ENV')             :- atom(V), 
                                         extend(ENV, V|->T, ENV').
                                         
declpat(ENV, (V^W)|->(TV><TW), ENV'') :- declpat(ENV,  V|->TV, ENV'),
                                         declpat(ENV', W|->TW, ENV'').

type   (ENV, succ(V), int)        :- .
declpat(ENV, succ(V)|->int, ENV') :- declpat(ENV, V|->int, ENV').
declpat(ENV, 0|->int, ENV)        :- .


rshow(E, T, TY) :- map_Lookup(debugging, 1), !, show(E, "|-", T, "::", TY).
rshow(E, T, TY) :- .

debug(on)  :- map_Enter(debugging, 1).
debug(off) :- map_Enter(debugging, 0).

Term:: Type :- type((inc|->(int->int))++(s|->string), Term, Type),
               show(Term, ":: ", Type).

prob1 :- f=>g=>x=>f[g[x]]          :: T.
prob2 :- f=>g=>x=>f[x]^g[x]        :: T.
prob3 :- succ(x)=>x+1 | 0=>0       :: T.
prob4 :- f=>(x=>f[x]|y=>f[f[y]])   :: T.
prob5 :- (x^y)=>((x+1)^ y)         :: T.
prob6 :- (let i=(x=>x) in i[s])     :: T.
prob7 :- (let i = x=>x in i[i])     :: T. 

prob8  :- (let i = x=>x; c = f=>g=>x=>f[g[x]] in c[i][i][s])            :: T.
prob8a :- (let i = x=>x; j = x=>x; c = f=>g=>x=>f[g[x]] in c[i][j][s])  :: T.
prob9  :- (let i = x=>x; c = f=>g=>x=>f[g[x]] in i^c)                   :: T. 
prob10 :- (let i = x=>x in i[inc])                                      :: T. 
prob11 :- (let i = x=>x in i[3])                                        :: T. 

eval(String, Type) :- toFormula(String, Formula), type(nil, Formula, Type).
#use interact











