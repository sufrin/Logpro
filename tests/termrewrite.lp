-- nth(N:Nat, XS:'a list, Result: 'a) 
-- Result is the Nth element of Xs
nth(0, X:XS, X) :- .
nth(succ(N), X:XS, Y) :- nth(N, XS, Y).

-- replnth(N:Nat, XS:'a list, X': 'a, Result:'a list)
-- Result is XS with Nth element equal to X'
replnth(0, X:XS, X', X':XS) :- .
replnth(succ(N), X:XS, X', X:XS') :- replnth(N, XS, X', XS').

-- select(Path:Nat list, T: Term, Result: Term)
-- Result is the subtree of T designated by Path
    select(nil,  T, T)  :- .
    select(N:NS, T, T') :-  struct(T),            -- \rmbox{it must be a structure}
                            functor(T, OP, Sts),  -- \rmbox{explode it}
                            nth(N, Sts, St),      -- \rmbox{get the \(N\)th subterm}
                            select(NS, St, T'). 
                            
-- struct(T:Term) is a non-atom, non-num, non-str
struct(T) :- not atom(T), not num(T), not str(T).
                        
-- assign(U: Term, Path: Nat list, T: Term, Result: Term)
-- Result is T except that select(Path, T, U) holds.
assign(U, nil,  T, U)  :- .
assign(U, N:NS, T, T') :- functor(T, OP, Sts),
                          nth(N, Sts, St),
                          assign(U, NS, St, St'),
                          replnth(N, Sts, St', Sts'),
                          functor(T', OP, Sts').
                          


-- a generic term-rewriting engine
#infix0 -->

-- rewrite(Tree: Term, Tree': Term)
-- Tree' is Tree with outermost occurence of a rule's 
--       Pattern rewritten as that rule's Replacement
rewrite(Tree, Tree') :-
        Pat --> Repl, 
        select(Path, Tree, Pat), 
        !,
        assign(Repl, Path, Tree, Tree').

-- rew(Tree:Term, Tree'': Term)
-- Tree'' is Tree repeatedly rewritten until no rewrites are possible
rew(Tree, Tree'') :- rewrite(Tree,   Tree'),
                     show(Tree, "-->"),
                     !, 
                     rew(Tree', Tree'').
                          
rew(Tree, Tree)   :- not rewriteable(Tree), 
                     show(Tree, ".").

rewriteable(Tree)   :- rewrite(Tree, Tree').
        
-- rulebase for the engine
(A+B)+C --> A+(B+C).
(A*B)*C --> A*(B*C).
0+X     --> X.
0*X     --> 0.
1*X     --> X.
X+0     --> X.
X*0     --> 0.
X*1     --> X.

-- test data
#infix4 +
#infix4 -
#infix5 *
#infix5 /

rew(T) :- rew(T, T').

p1 :- rew(((a+b)+0)+d).
p2 :- rew(((0+b)+c)+d*1*((f+g)+h)).
p3 :- rew(((0+b)+1*c+0)+d*1*((f+g)+h)).
        


