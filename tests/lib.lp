/*
        $Id: lib.lp,v 1.3 2000/01/12 10:24:00 sufrin Exp $ 
        
        Some essential primitive predicates
*/
#infix3 "=" 
#infix3 "==" 
#infix3 "/=" 

X==X        :- .
X=X         :- .
X/=Y        :- not X=Y.            
diff(X,Y,R) :- sum(Y, R, X).
quot(X,Y,R) :- times(Y, R, X).








