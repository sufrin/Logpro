(*
        $Id: builtin.ml,v 1.15 2000/01/14 10:37:07 sufrin Exp $
        
        Implementations of the builtin LogPro relations
        
*)
open Syntax
open Printer
open Subst
open Unify
open Util
open List
open Format
  
  let cansolve: form -> bool =
  function 
  |  Num    _  -> false
  |  String _  -> false
  |  Var    _  -> false
  |  Struct(a, fs) -> 
  match (a, length fs) with
  | "sum",        3  -> true
  | "succ",       2  -> true
  | "prod",       3  -> true
  | "fail",       0  -> true
  | "write",      _  -> true
  | "show",       _  -> true
  | "num",        1  -> true
  | "var",        1  -> true
  | "nonvar",     1  -> true
  | "str",        1  -> true
  | "toString",   2  -> true
  | "toFormula",  2  -> true
  | "functor",    3  -> true
  | "atom",       1  -> true
  | "hd",         2  -> true
  | "tl",         2  -> true
  | "len",        2  -> true
  | "cat",        3  -> true
  | "read",       1  -> true
  | "ascii",      2  -> true
  | ">",          2  -> true
  | ">=",         2  -> true
  | "<",          2  -> true
  | "<=",         2  -> true
  | "map_Lookup", 2  -> true
  | "map_Enter",  2  -> true
  | "map_Add",    2  -> true
  | "map_All",    2  -> true
  | "map_Remove", 1  -> true
  | _,            _  -> false
  

  let logNil = Struct("nil", [])
  
  let to_loglist: form list -> form =
  fun forms -> List.fold_right 
                 (fun t l -> Struct(":", [t; l]))
                 forms
                 logNil
                 
  let rec from_loglist: form -> form list =
  function
  | Struct("nil", [])     -> []
  | Struct(":",   [t; l]) -> t::from_loglist l
  | _                     -> raise Fail
                 
  let form_of_string: string -> form =
  fun string -> 
      try  Parser.formula Scanner.token (Lexing.from_string string)
      with _ -> raise Fail
  
  let solve: (subst -> unit) -> form -> subst -> unit =
  fun action goal subst ->
  let goal  = apply subst goal in
  let err m = raise(Error(m, goal))
  in
  let arith op invop = function 
      |  [Num m; Num n; Num p] -> if op n m=p then subst else raise Fail
      |  [Num m; Num n; Var v] -> extend subst v (Num(op n m))
      |  [Num m; Var v; Num n] -> extend subst v (Num(invop n m))
      |  [Var v; Num m; Num n] -> extend subst v (Num(invop n m))
      |  _ -> err "Arithmetic functions need at least two integers: "
  and reln rel = function
      |  [Num n; Num m] -> if rel n m then subst else raise Fail
      |  _ -> err "Arithmetic comparisons need two integers: "
  and succ  = function
      |  [Num n; Num m] when m=n+1 -> subst 
      |  [Var v; Num m] when m>0   -> extend subst v (Num (m-1))
      |  [Num n; Var v] when n>=0  -> extend subst v (Num (n+1))
      |  [Var _; Var _] -> err "Succ needs at least one natural number: "
      |  _              -> raise Fail
  and fromstring f = function
      |  [String s; r] -> unify subst r (f s)
      |  _             -> err "String function needs a string as first argument: "
  and tostring = function
      |  [f; r] -> unify subst r (String(sprintform f)) 
      |  _      -> err "Function needs a ground formula as first argument: "
  and toformula = function
      |  [String s; r] -> unify subst r (form_of_string s) 
      |  _      -> err "Function needs a string as first argument: "
  and cat = function
      |  [String s; String t; u] -> unify subst u (String(s^t))
      |  [String s; u; String t] -> unify subst u (String(removeprefix s t))
      |  [u; String s; String t] -> unify subst u (String(removesuffix s t))
      |  _                       -> err "String cat needs at least two strings: "
  and ascii = function
      |  [String s; n] -> if s<>"" then
                             unify subst n (Num(Char.code(String.get s 0)))
                          else err "Ascii needs a nonempty string and/or a number."
      |  [s; Num n]    -> if (32<=n && n<=127) || n=10 then 
                             unify subst s (String(String.make 1 (Char.chr n)))
                          else err "Ascii code out of range 10, 32..127."
      |  _             -> err "Ascii needs a String and/or a Number: "
  and lookup = fun 
         key value -> 
         try unify subst value (Db.lookup key) with Not_found -> raise Fail
  and functor_rel = function
      |  [Struct(f, args); opr; res] -> 
         let subst' = unify subst (String f) opr in
             unify subst' res (to_loglist args)
      |  [v; Struct(opr, []); res] ->
             unify subst v (Struct(opr, from_loglist res))
      |  [v; String f; res] ->
             unify subst v (Struct(f, from_loglist res))
      |  _      -> err "Functor has bad arguments: "
  in 
  try
      let subst' = match goal with
      |   Struct("sum",   args)         -> arith (fun x y->x+y) (fun x y->x-y) args 
      |   Struct("succ",  args)         -> succ args 
      |   Struct("prod",  args)         -> arith (fun x y->x*y) (fun x y->x/y) args
      |   Struct(">=",    args)         -> reln (fun x y -> x>=y) args 
      |   Struct(">",     args)         -> reln (fun x y -> x>y)  args
      |   Struct("<=",    args)         -> reln (fun x y -> x<=y) args
      |   Struct("<",     args)         -> reln (fun x y -> x<y)  args
      |   Struct("write", args)         -> printf "@[%a@]@?" (formatlist "@;" formatshow) args; subst
      |   Struct("show",  args)         -> printf "@[%a@]@." (formatlist "@;" formatshow) args; subst
      |   Struct("read",  [res])        -> unify subst res (String (read_line()))
      |   Struct("hd",    args)         -> fromstring (fun s -> String(String.sub s 0 1)) args
      |   Struct("tl",    args)         -> fromstring (fun s -> String(String.sub s 1 (String.length s - 1))) args
      |   Struct("len",   args)         -> fromstring (fun s -> Num(String.length s)) args
      |   Struct("cat",   args)         -> cat args
      |   Struct("ascii", args)         -> ascii args
      |   Struct("fail",  [])           -> raise Fail
      |   Struct("var",        [Var v]) -> subst 
      |   Struct("var",        [_])     -> raise Fail 
      |   Struct("nonvar",     [Var v]) -> raise Fail 
      |   Struct("nonvar",     [_])     -> subst 
      |   Struct("num",        [Num _]) -> subst 
      |   Struct("num",        [_])     -> raise Fail 
      |   Struct("toString",   args)    -> tostring args 
      |   Struct("toFormula",  args)    -> toformula args 
      |   Struct("str",        [String _])     -> subst 
      |   Struct("str",        [_])            -> raise Fail 
      |   Struct("atom",       [Struct(_,[])]) -> subst 
      |   Struct("atom",       [_])            -> raise Fail 
      |   Struct("functor",    args)           -> functor_rel args
      |   Struct("map_Lookup", [k;v])          -> lookup k v
      |   Struct("map_Enter",  [k;v])          -> (Db.enter k v; subst)
      |   Struct("map_Add",    [k;v])          -> (Db.add  k v;  subst)
      |   Struct("map_Remove", [k])            -> (Db.remove k;  subst)
      |   Struct("map_All",    [k;v])-> unify subst v (to_loglist(Db.findall k))
      |   _                          -> raise (Error ("Internal error: ", goal))
      in  action subst'
  with Fail               -> ()
  |    Invalid_argument _ -> ()















