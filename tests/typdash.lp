        #use lookup
        #infix0 |-
        #infix1 ::
        #infix2 where
        #infix4 =>        
        #infix4 =          
        #infix3 ->     
        #infix6 +
        #infix6 -
        
        ENV |- N        :: int   :- num(N).
        ENV |- V        :: T     :- variable(V), lookup(V, ENV, T).
        ENV |- E+E'     :: int   :- ENV |- E  :: int, 
                                    ENV |- E' :: int.
        ENV |- Bv=>Body :: T->T' :- extend(ENV, Bv |-> T, ENV'),
                                    ENV' |- Body :: T'.
        ENV |- Body where Bv=E   :- ENV |- E :: T,
                                    extend(ENV, Bv |-> T, ENV'),
                                    ENV' 
        
        
        
        
        variable(V) :- atom(V).
        
        
