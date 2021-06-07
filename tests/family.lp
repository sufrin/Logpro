/*
        Family life
        
        $Id: family.lp,v 1.3 2000/01/17 01:06:37 sufrin Exp $
        
*/

parent(sid,       bill).
parent(lizzie,    bill).
parent(sid,       lesley).
parent(lizzie,    lesley).
parent(lesley,    winifred).
parent(lesley,    albert).
parent(bill,      gerald).
parent(bill,      sarah).
/*
parent(david,     gerald).
parent(david,     sarah).
parent(lorraine,  gerald).
parent(lorraine,  sarah).
parent(sarah,     loretta).
parent(sarah,     joseph).
parent(edna,      loretta).
parent(edna,      joseph).
parent(gerald,    samuel).
parent(gerald,    francesca).
parent(muriel,    samuel).
parent(muriel,    francesca).
parent(frances,   muriel).
parent(frances,   joe).
parent(robert,    muriel).
parent(robert,    joe).
parent(derek,     winifred).
parent(derek,     albert).
parent(manon,     derek).
parent(manon,     susie).
parent(lucy,      derek).
parent(lucy,      susie).
parent(sara,      david).
parent(emily,     david).
parent(sara,      gina).
parent(emily,     gina).
parent(gina,      alain).
parent(gina,      francoise).
parent(samuel,    otto).
parent(samuel,    louisa).
*/

male(sid).
male(bill).
male(gerald).
male(joe).
male(joseph).
male(samuel).
male(david).
male(robert).
male(derek).

person(X) :- parent(X, Y).
person(X) :- parent(Y, X).

#use lib
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
dad(X, Y)         :- parent(X, Y), male(Y).
mum(X, Y)         :- parent(X, Y), female(Y).

sibling(X, Z)     :- parent(X, Y), parent(Z, Y), X/=Z.
cousin(X, Z)      :- grandparent(X, Y), grandparent(Z, Y), X/=Z.
ancestor(X, Y)    :- parent(X, Y).
ancestor(X, Y)    :- parent(X, Z), ancestor(Z, Y).


ancestor1(X, Z) :- parent(X, Z).
ancestor1(X, Z) :- parent(X, Y), ancestor1(Y, Z).

ancestor2(X, Z) :- parent(X, Y), ancestor2(Y, Z).
ancestor2(X, Z) :- parent(X, Z).

ancestor3(X, Z) :- parent(X, Z).
ancestor3(X, Z) :- ancestor3(X, Y), parent(Y, Z).

ancestor4(X, Z) :- ancestor4(X, Y), parent(Y, Z).
ancestor4(X, Z) :- parent(X, Z).


