(*
        $Id: subst.ml,v 1.5 2000/01/02 00:10:42 sufrin Exp $
        
        Substitutions
*)
open Syntax
open Printer
open Util

type subst = (name*form) list

let formatbinding  fmt (name, form) = 
    Format.fprintf fmt "@[%a = %a@]" formatName name formatform form

let formatsubst fmt = 
    Format.fprintf fmt "@[%a@]" (formatlist "@;" formatbinding)

let formatanswer fmt = 
    function [] -> Format.fprintf fmt "yes"
    |        sb -> Format.fprintf fmt "@[%a@]" (formatlist "@;" formatbinding) sb

let empty: subst = []

let extend: subst -> name -> form -> subst=
fun s n f -> (n, f) :: s
    
let rec lookup: name -> subst -> form  =
fun n ->
function []           -> failwith "lookup"
|        (n', f):: s' -> if n=n' then f else lookup n s'


let rec lookupname: subst -> name -> form -> form  =
fun s n result ->
try let f' = lookup n s 
    in  match f' with
    |   Var n' -> lookupname s n' f'
    |   f'     -> f'
with 
    Failure _ -> result
        
let lookupterm: subst -> form -> form =
fun subst form ->
    match form with
    |     Var name -> lookupname subst name form
    |     _        -> form
    
let apply: subst -> form -> form =
let rec ap =
fun names subst form ->
    match form with
    | Var name      -> 
      let form' = lookupname subst name form 
      in  if List.mem name names then
             form'
          else
          (match form' with 
             Struct (_,_) -> ap (name::names) subst form' 
          |  _            -> form')
    | Struct(a, fs) -> Struct(a, List.map (ap names subst) fs)
    | _             -> form
in  ap []

let close: subst -> subst = 
fun subst -> List.map (fun (i, t) -> (i, apply subst t))  subst

let restrict: name list -> subst -> subst =
fun vs subst -> List.filter (fun (v, f) -> List.mem v vs) subst







