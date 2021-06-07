(*
        $Id: trace.ml,v 1.6 2000/01/17 01:06:17 sufrin Exp $
        
        Tracing 
*)
open Syntax
open Printer
open Format
open Subst
open List

let subgoals = ref false
let subst    = ref false
let showsubgoals: int -> subst -> clause -> form -> unit =
fun level s (con, antes) goal ->
if  !subgoals then 
    let clause = (apply s con, map (apply s) antes) in
        eprintf "@[:: %d @[%a@]@]@." level formatclause clause; 
        if !subst then eprintf "@[:: @[%a@]@]@." formatsubst  (close s);
        if Util.stop ">> " then raise Util.Backtrace
                                       





