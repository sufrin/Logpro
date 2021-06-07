#use lists
#infix1 |
#prefix leaf

appdl(As|Bs, Bs|Cs, As|Cs) :- .

condl(As|Bs, As) :- var(Bs).
condl(As|Bs, R)  :- nonvar(Bs), append(R, Bs, As).

flatten(leaf X, X:End|End).
flatten(T1 + T2, R) :- flatten(T1, L1), flatten(T2, L2), appdl(L1, L2, R).

fl(Tree, List) :- flatten(Tree, List|nil).

flat(leaf X, X:nil).
flat(T1 + T2, R) :- flat(T1, L1), flat(T2, L2), append(L1, L2, R).

/* Make an open list from a regular list */
dl(nil, V, V).
dl(X:XS, V, X:R)  :- dl(XS, V, R).
dl(XS, R'|V)      :- dl(XS, V, R').

#count on
prob1(R) :- flat(leaf a+leaf b + leaf c + leaf d + leaf e + leaf f, R).
prob2(R) :- flat((leaf a+leaf b + leaf c) + (leaf d + leaf e + leaf f), R).
prob3(R) :- fl(leaf a+leaf b + leaf c + leaf d + leaf e + leaf f, R).
prob4(R) :- fl((leaf a+leaf b + leaf c) + (leaf d + leaf e + leaf f), R).
prob5(R) :- flat((((((leaf a+leaf b) + leaf c) + leaf d) + leaf e) + leaf f), R).
prob6(R) :- fl((((((leaf a+leaf b) + leaf c) + leaf d) + leaf e) + leaf f), R).
prob7(R) :- flat(leaf a+(leaf b + (leaf c + (leaf d + (leaf e + leaf f)))), R).
prob8(R) :- fl(leaf a+(leaf b + (leaf c + (leaf d + (leaf e + leaf f)))), R).


