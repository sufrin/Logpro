(*
        $Id: db.ml,v 1.11 2000/01/12 10:23:47 sufrin Exp $
        
        Clause database     
*)
open Syntax
open Format
open Hashtbl
open Util

let map = List.map
and length = List.length

type hashtable = (Syntax.atom * int, Syntax.clause list ref) Hashtbl.t

let  database: hashtable = Hashtbl.create 123

let  addClause clause =
let  (con, ante) = clause in
     match con with
     | Var _ | Num _ | String _ ->
         raise (ClauseError("Consequents must be atoms or structures: ", clause))
     | Struct(name, ts) ->
        List.iter 
          ( function  
          | Struct _ -> () 
          | form     -> raise (ClauseError("Antecedents must be atoms or structures: ", clause))) 
          ante; 
        try 
         let clauses = find database (name, length ts) in 
             if List.mem clause (!clauses)
             then raise (ClauseError("Duplicate clause: ", clause))
             else clauses := !clauses @ [clause]
        with
         Not_found -> add database (name, length ts) (ref [clause])
   
let count = ref 0

let next () = (count:=1 + !count; !count)

let reset() = count := 0
   
let freshClause (con, antes) =
let n = next() in
let f = Syntax.fresh n
in  (f con, List.map f antes)
   
let freshClauses: form -> clause list =
function 
|   Struct(name, ts) ->
    let clauses = 
        try  !(find database (name, length ts)) 
        with Not_found ->
             if   ts=[] then 
                  (* a non-asserted simple proposition -- false *)
                  [] 
             else 
                  (* an unknown relation mentioned erroneously *)
                  raise(NotFound("Unknown relation: "^name^"/"^string_of_int(length ts)))
    in  map  freshClause clauses
    
|   form -> raise (Error("A query must be an atom or a structure: ", form))

let lookupTable: (Syntax.form, Syntax.form) Hashtbl.t = Hashtbl.create 123

let lookup form = find lookupTable form
let enter  form value = 
    (try remove lookupTable form with _ -> ());
    add lookupTable form value

let add     form value = add      lookupTable form value
let remove  form       = remove   lookupTable form 
let findall form       = find_all lookupTable form


