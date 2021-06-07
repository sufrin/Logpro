#prefix leaf
mintip(leaf X, Val, Val)   :- Val < X.
mintip(leaf X, Val, X)     :- Val >= X.
mintip(T1 ** T2, Val, Res) :- mintip(T1, Val, Val'), mintip(T2, Val', Res).

--      mintip(leaf 1**leaf 2**leaf 3**leaf 4, 0, R)?

mt(leaf N,   N,   X,   leaf X)   :- .
mt(T1 ** T2, Val, X,   T1'**T2') :- mt(T1, Val',  X, T1'), 
                                    mt(T2, Val'', X, T2'),
                                    min(Val', Val'', Val).

mt(T, R) :- mt(T, V, V, R).

min(V, W, V) :- V<W.
min(V, W, W) :- V>=W.

select(nil, T, T).
select(l:XS, T'**T'', T) :- select(XS, T', T).
select(r:XS, T'**T'', T) :- select(XS, T'', T).

assign(U, nil,   T,  U).
assign(U, l:XS,  T'**T'', V**T'') :- assign(U, XS, T',  V).
assign(U, r:XS,  T'**T'', T'**V)  :- assign(U, XS, T'', V).

#infix0 -->

rewrite(Tree, Tree') :-
        rule(Pat --> Repl), 
        select(Path, Tree, Pat), 
        assign(Repl, Path, Tree, Tree'),
        show(Tree, "==>", Tree').
        
rewrites(Tree, Tree).
rewrites(Tree, Tree') :- rewrite(Tree, Tree''), 
                         rewrites(Tree'', Tree').

        
rule(X ** (Y ** Z) --> X ** Y ** Z).
rule(X ** X --> X).

test(leaf a**(leaf b**(leaf c**leaf d))**leaf e).
test(leaf a**leaf a**(leaf a**(leaf c**leaf d))**leaf e).

        
