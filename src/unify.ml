(*
        $Id: unify.ml,v 1.12 1999/12/01 14:34:51 sufrin Exp $
        
        Unification
*)
open Syntax
open Printer
open Util
open Subst

let occurscheck = ref true

let assign subst v f =
    if !occurscheck && occurs v f then
       raise Fail
    else 
       extend subst v f
 
let rec unify: subst -> form -> form -> subst =
fun subst f1 f2 ->
 let f1 = lookupterm subst f1
 and f2 = lookupterm subst f2
 in
 match  f1, f2 with
 |   Struct(a1, fs1), Struct(a2, fs2) -> 
       if   a1=a2 
       then unifyforms subst fs1 fs2 
       else raise Fail
 |   Num n1,    Num n2    -> if n1=n2 then subst else raise Fail
 |   String n1, String n2 -> if n1=n2 then subst else raise Fail
 |   Var n1,    Var n2    -> if n1=n2 then subst else extend subst n1 f2
 |   Var n1,    _         -> assign subst n1 f2 
 |   _,         Var n2    -> assign subst n2 f1
 |   _                    -> raise Fail
          
and unifyforms: subst -> form list -> form list -> subst =
fun subst fs1 fs2 ->
    match fs1, fs2 with
      [],    []    -> subst
    | x::xs, y::ys -> unifyforms (unify subst x y) xs ys
    | _            -> raise Fail
    













    











