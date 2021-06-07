(* 
        $Id: syntax.ml,v 1.13 1999/12/03 11:37:10 sufrin Exp $ 
        
        
        Abstract Syntax of formulae and commands
*)

type    name       = int * string
let     mkname  s  = (0, s)
type    atom       = string
type    num        = int

type    form    =  Var    of name
        |          Struct of atom * term list
        |          Num    of int
        |          String of string

and     clause  =  form * form list

and     term    = form    (* convenient to share representations *)

type    cmd     =  Clause    of clause
        |          Query     of form list
        |          Report    of string
        |          Directive of string list
        |          Eof
        
(*
        Renaming of variables in formulae
*)

let fresh n' =
    let rec ren = 
    function Var(n, s)     -> Var(n', s)
    |        Struct(a, ts) -> Struct(a, List.map ren ts)
    |        other         -> other
    in ren
    
(*
        A ground formula has no variables
*)
let rec ground: form -> bool =
function Var _         -> false
|        Struct(_, ts) -> List.for_all ground ts
|        _             -> true

(*      
        Find the variables in a list of form
*)
let vars: form list -> name list =
fun forms ->
  let rec mergewith form vars =
    match form with
    |   Var v         -> if List.mem v vars then vars else v::vars
    |   Struct(_, ts) -> List.fold_right mergewith ts vars 
    |   _             -> vars
  in
    List.fold_right mergewith forms [] 
    
let rec occurs: name -> form -> bool =
fun n ->
    function (Var n') when n=n' -> true
    |        (Struct(_, ts))    -> List.exists (occurs n) ts
    |        _                  -> false
    

