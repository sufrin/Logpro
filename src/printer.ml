(*
        $Id: printer.ml,v 1.10 2000/02/07 00:27:54 sufrin Exp $
        
        Formula, Term, and Command printing
*)
open Syntax
open Format

let isinfix           = Scanner.isinfix 
let isprefix          = Scanner.isprefix 
let bracketleftwards  = Scanner.bracketleftwards
let bracketrightwards = Scanner.bracketrightwards
 
let formatName fmt (n, s) = 
    if   n=0 
    then fprintf fmt "%s" s 
    else fprintf fmt "%s.%d" s n
    
let rec formatlist punct formatitem fmt =
function []    -> ()
|        [s]   -> formatitem fmt s
|        s::ss -> formatitem fmt s;
                  fprintf fmt punct;
                  formatlist punct formatitem fmt ss
   
let formatstrings  = formatlist "@;<1 1>" pp_print_string

let rec formatcmd fmt f = match f with
|  Clause c     -> formatclause fmt c
|  Query f      -> fprintf fmt ":- %a@." (formatlist ",@;" formatform) f
|  Report s     -> fprintf fmt "%s@." s
|  Directive ss -> fprintf fmt "@[#%a.@]@." formatstrings ss
|  Eof          -> fprintf fmt "@." 
        
and formatclause fmt (con, ante) =
    fprintf fmt "@[%a :-@;<1 4>@[<hov> %a.@]@]" formatform con formatforms ante

and formatform fmt f = match f with
|   Var    name    -> fprintf fmt "%a" formatName name
|   Num    n       -> fprintf fmt "%d" n
|   String s       -> fprintf fmt "\"%s\"" s
|   Struct ("{}", ts) -> fprintf fmt "{%a}" formatforms ts
|   Struct ("[]", [t1; t2]) -> fprintf fmt "%a[%a]" formatform t1 formatform t2
|   Struct ("[||]", [t1; t2]) -> fprintf fmt "[| %a |] %a" formatform t1 formatform t2
|   Struct (f, ts) -> 
    match f with
    | "[]" | "[||]" -> fprintf fmt "(%s)(%a)" f formatforms ts
    | _ -> 
    match ts with 
    |  [t1; t2] when isinfix f  -> fprintf fmt "%a %s %a" (fls f) t1 f (frs f) t2
    |  [t1]     when isprefix f -> fprintf fmt "%s %a"    f (frs f) t1
    |  [] -> fprintf fmt "%s" f
    |  _  -> fprintf fmt "%s(%a)" f formatforms ts
        
and formatforms fmt = formatlist ",@, " formatform fmt

and fls c fmt f = match f with
|   Struct (a, [_;_]) when bracketleftwards a c -> fprintf fmt "(%a)" formatform f 
|   _ -> formatform fmt f
     
and frs c fmt f = match f with
|   Struct (a, [_;_]) when bracketrightwards c a -> fprintf fmt "(%a)" formatform f 
|   _ -> formatform fmt f

and formatrform fmt f = match f with
|   Struct("[]", _) -> fprintf fmt "(%a)" formatform f
|   _               -> formatform fmt f

let formatshow fmt =
function String s -> fprintf fmt "@[%s@]" s
|        other    -> fprintf fmt "@[%a@]" formatform other
     
let rec sprintform =
function  String s              -> s
|         Num n                 -> string_of_int n
|         Struct("{}", ts)      -> "{"^sprintforms ts^"}"
|         Struct("[]", [t1;t2]) -> sprintform t1^"["^sprintform t2^"]"
|         Struct(a, [])         -> a
|         Struct(a, ts)         -> a^"("^sprintforms ts^")"
|         Var(n, s)             -> sprintf "%s.%d" s n

and sprintforms =
function [f]   -> sprintform f
|        f::fs -> sprintform f^", "^sprintforms fs



















