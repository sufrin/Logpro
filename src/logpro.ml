(*
        $Id: logpro.ml,v 1.44 2000/02/06 23:21:07 sufrin Exp $

        Logic programming language
*)

open Syntax 
open Unify
open Subst
open Format
open Printer
open Db
open Util

let version = Version.version

let  inferences      = ref 0
let  countinferences = ref false

type action = subst -> unit

exception Prune
exception Cut of int
exception Success
let success: subst -> 'a = fun _ -> raise Success

let rec tryClauses: clause list -> (clause -> action) -> action =
fun clauses solve subst ->
try
 List.iter (fun clause -> 
             try solve clause subst with 
             |    Fail       -> ()
           ) clauses
with Prune -> () 


let  subgoals: subst -> clause -> form -> (subst * form list) =
fun  subst (cons, antes) goal ->
     let subst' = unify subst cons goal 
     in  (subst', List.map (apply subst') antes)
 
let rec solveAnd: int -> action -> form list -> subst -> unit =
fun level action forms subst ->
    match forms  with 
    |  []          -> action subst
    |  Struct("!", [])::goals -> (solveAnd level action goals subst; raise (Cut level))
    |  goal::goals -> solveFormula level (solveAnd level action goals) goal subst 

and solveFormula: int -> action -> form -> subst -> unit =
fun level action goal subst ->
    let level = level+1 in
    if   Builtin.cansolve goal 
    then Builtin.solve action goal subst 
    else 
    match goal with
    | Struct("call", goals)  -> solveAnd level action (List.map (apply subst) goals) subst
    | Struct("not",  [goal]) -> solveNot level action subst goal
    | _ -> tryClauses (Db.freshClauses goal) (solveFormulaWith level action goal) subst 
           
and solveFormulaWith: int -> action -> form -> clause -> subst -> unit =
    fun level action goal clause subst  ->
    let (subst', goals') = subgoals subst clause goal
    in  Trace.showsubgoals level subst' clause goal;
        inferences := 1 + !inferences;
        try
          solveAnd level action goals' subst' 
        with 
        |  Error (why, culprit) ->
            (eprintf "@[%d in %a@]@." level formatform (apply subst goal);
             raise (Error (why, culprit)))
        |  Backtrace ->
            (eprintf "@[%d in %a@]@." level formatform (apply subst goal);
             raise Backtrace)
        |  Cut level' -> 
             (* eprintf "@[Cutting %d at %d in %a@]@." level' level formatform (apply subst goal); *)
             if level' < level then raise(Cut level') else raise Prune

and solveNot: int -> action -> subst -> form -> unit =
fun level action subst goal ->
 let goal = apply subst goal in
 if ground goal then
    try solveFormula level success goal subst; raise Fail
    with 
    |  Success    -> raise Fail
    |  Fail       -> action subst
 else
    raise (Error("This negated formula should be a ground formula: ", goal))


(*
        Solutions
        
*)
exception Stop

let showsolutions: form list -> unit =
fun forms -> 
    let showsolution: form list -> subst -> unit =
    fun forms subst -> 
        if !countinferences then printf "@[After %d inferences@] @." (!inferences);
        printf "@[%a@] @?" formatanswer (restrict (vars forms) (close subst)) ;
        if Util.stop "? " then raise Stop
    in inferences := 0;
       try  
         solveAnd 0 (showsolution forms) forms empty
       with 
       | Backtrace        -> ()
       | Fail             -> ()
       | Stop             -> ()
       | Cut n            -> (eprintf "@[[[Unexpected cut %d]]@]@." n)
       
let used: string list ref = ref []

open Lexing
open Format

let currentprompt = ref ""

let from_channel chan prompt =
    let flexing = Lexing.from_channel chan
    in
    let refill lexing = 
        if !currentprompt <> "" then begin
           fprintf std_formatter "%s@?" (!currentprompt);
           currentprompt := ">> ";
        end;
        flexing.refill_buff lexing
    in  currentprompt := prompt; { flexing with refill_buff = refill }
    
(*
let from_channel chan prompt =
    let flexing = Lexing.from_channel chan 
    in
    let refill lexing = 
        if prompt <> "" then fprintf std_formatter "%s@?" prompt;
        flexing.refill_buff lexing
    in  { flexing with refill_buff = refill }
*)
    

let parseFrom: in_channel -> string -> unit -> cmd  =
fun chan prompt -> 
    let lexbuf = from_channel chan prompt in
    fun () ->
    try 
        Parser.cmd Scanner.token lexbuf
    with
    |   Failure why -> Report (why^" at line "^Scanner.position())
    |   _           -> Report ("Syntax error at line "^Scanner.position())
    
let rec interpretfile path prompt instream =
    let 
        readcommand = parseFrom instream prompt
    in  Scanner.file path;
        while 
          currentprompt := prompt;
          let cmd = readcommand () in
              match cmd with
              |  Eof          -> false
              |  Clause    c  -> protect Db.addClause c; true
              |  Query     q  -> Db.reset(); protect showsolutions q; true
              |  Directive ss -> protect directive ss; true 
              |  Report s     -> eprintf "@[%s@]@." s; (path="<stdin>") 
        do
          ()
        done;
        if path <> "<stdin>" then begin
                close_in instream;
                Scanner.endfile()
        end

and directive: string list -> unit  = 
function
|       ["#use";   path]  -> usefile path
|       ["#count"; "on"]  -> countinferences := true
|       ["#count"; "off"] -> countinferences := false
|       ["#trace"; "all"] -> Trace.subgoals := true; Trace.subst := true
|       ["#trace"; "on"]  -> Trace.subgoals := true; Trace.subst := false
|       ["#trace"; "off"] -> Trace.subgoals := false
|       ["#check"; "on"]  -> Unify.occurscheck := true
|       ["#check"; "off"] -> Unify.occurscheck := false
|       ["#infix0"; sym]  -> Scanner.newInfix sym 0
|       ["#infix1"; sym]  -> Scanner.newInfix sym 1
|       ["#infix2"; sym]  -> Scanner.newInfix sym 2
|       ["#infix3"; sym]  -> Scanner.newInfix sym 3
|       ["#infix4"; sym]  -> Scanner.newInfix sym 4
|       ["#infix5"; sym]  -> Scanner.newInfix sym 5
|       ["#infix6"; sym]  -> Scanner.newInfix sym 6
|       ["#infix7"; sym]  -> Scanner.newInfix sym 7
|       ["#infix8"; sym]  -> Scanner.newInfix sym 8
|       ["#prefix"; sym]  -> Scanner.newPrefix sym
|       ["#notfix"; sym]  -> Scanner.newNotfix sym
|       ["#width";  n]    -> Format.set_margin (intfrom "#width needs a number" n)
|       other             -> eprintf "I don't understand that directive.@." 

and usefile path =
    try 
        match path with
        | "-"   ->  interpretfile "<stdin>" "-- " stdin
        | "-v"  ->  exit 0
        | ""    ->  ()
        | _     ->  
        match String.get path 0 with
        | '-' -> eprintf "Usage: logpro [-v] [filenames (default extension .lp)]@."; exit 2
        |  _  -> if List.mem path (!used) then
                    ()
                 else begin
                    interpretfile path "" (try open_in (path^".lp") with Sys_error _ -> open_in path);
                    used := path:: !used
                 end
     with 
        Sys_error why -> eprintf "[[ %s ]]@." why 

let main argv =
    Format.set_margin 80;
    eprintf "%s@." version; 
    match argv with
    | []    -> ()
    | p::ps -> (List.iter usefile ps; usefile "-")
   
let _ = main(Array.to_list(Sys.argv)) 














































