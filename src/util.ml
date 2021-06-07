(*
        $Id: util.ml,v 1.11 1999/12/23 00:55:50 sufrin Exp $
*)
open Syntax
open Printer
open Format

exception Fail                           (* from unify, etc          *) 
exception Backtrace                      (* from continuation prompt *)
exception Error       of string * form   (* from builtin             *)
exception ClauseError of string * clause (* from database            *)
exception NotFound of string             (* from database            *)

let protect: ('a -> unit) -> 'a -> unit =
fun f a ->
    try 
       Sys.catch_break true; 
       f a 
    with 
       Sys.Break                   -> eprintf "[[ Interrupted ]]@."
    |  Failure why                 -> eprintf "[[ %s ]]@." why
    |  NotFound why                -> eprintf "[[ %s ]]@." why
    |  Stack_overflow              -> eprintf "[[ Stack overflow (cyclical solution?) ]]@."
    |  Error        (why, culprit) -> eprintf "@[[[ %s@;%a ]]@]@." why formatform culprit;
    |  ClauseError  (why, culprit) -> eprintf "@[[[ %s@;%a ]]@]@." why formatclause culprit;
    Sys.catch_break false
    
let intfrom: string -> string -> int =
fun why s -> 
    try int_of_string s with _ -> raise(Failure why)
    
let readline() = try read_line() with End_of_file -> "."

let rec stop prompt =
    printf "%s@?" prompt;
    match readline() with
    | "." -> true
    | ""  -> false
    | _   -> (printf "`.' to stop; <newline> to continue.\n"; stop prompt)
          
     

open String

let removeprefix s t =
    if length s <= length t && sub t 0 (length s) = s then 
       sub t (length s) (length t - length s)
    else raise Fail

let removesuffix s t = 
    if length s <= length t && sub t (length t - length s) (length s) = s then 
       sub t 0 (length t - length s)  
    else raise Fail











