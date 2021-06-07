        #use lookup
        #infix0 |-
        #infix0 ||-
        #infix1 ::
        #infix2 where
        #infix4 =>        
        #infix4 =          
        #infix3 ->     
        #infix6 +
        #infix6 -
        
        Env |- N        :: int   :- num(N).
        Env |- V        :: T     :- variable(V), lookup(V, Env, T).
        Env |- E+E'     :: int   :- Env ||- E  :: int, 
                                    Env ||- E' :: int.

        Env |- Bv=>Body :: T->T' :- extend(Env, Bv|->T, Env'),
                                    Env' ||- Body :: T'.
                                    
        Env |- Body where F[Bv]=E :: T :- !, Env |- Body where F=Bv=>E :: T.
                                    
        Env |- Body where V=E :: T' :-  Env ||- E :: T,
                                        extend(Env, V|->T, Env'),
                                        Env' ||- Body :: T'.
                                        
        Env |- F[A] :: T' :- Env ||- F :: T->T',
                             Env ||- A :: T.
        
        
        
        Env ||- Expr :: Type :-        show("[[[", Env |- Expr :: Type),
                                       Env |- Expr :: Type, 
                                       show(Env |- Expr :: Type, "]]]").
        
        variable(V) :- atom(V).
        


