#use lists
lookup(A, nil, Y)          :- fail.
lookup(X, (X|->Y):Dict, Y) :- !.
lookup(A, (X|->Y):Dict, B) :- lookup(A, Dict, B).

extend(Dict, X|->Y, (X|->Y):Dict) :- .

update(X|->Y, nil, X|->Y:nil) :- .
update(X|->Y, (A|->B):Dict, (X|->Y):Dict)  :- A=X.
update(X|->Y, (A|->B):Dict, (A|->B):Dict') :- A/=X, update(X|->Y, Dict, Dict').



