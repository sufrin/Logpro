{
        (* $Id: scanner.mll,v 1.25 2000/02/06 23:21:07 sufrin Exp $ *)
        
        open Parser
        open Syntax
        
        let mkstring s = STRING(String.sub s 1 (String.length s - 2))
        
        let symbols: (string, token) Hashtbl.t = Hashtbl.create 123
        
        let entersymbol: string -> token -> token =
        fun sym token ->
            Hashtbl.add symbols sym token; token
            
        let enter: string -> token -> unit =
        fun sym token ->
            Hashtbl.add symbols sym token
            
        let newPrefix: string -> unit =
        fun sym ->
            Hashtbl.remove symbols sym;
            Hashtbl.add    symbols sym (PREFIX sym)
            
        let newNotfix: string -> unit =
        fun sym ->
            Hashtbl.remove symbols sym;
            Hashtbl.add    symbols sym (NOTFIX sym)

        let _ = enter "{}"    (ATOM   "{}"); 
                enter "[]"    (ATOM   "[]");
                enter "[||]"  (ATOM   "[||]");
                enter "!"   (ATOM   "!");
                enter "nil" (ATOM   "nil");
                enter ">"   (INFIX4   ">");
                enter ">="  (INFIX4   ">=");
                enter "<"   (INFIX4   "<");
                enter "<="  (INFIX4   "<=");
                enter ":"   (INFIX7   ":");
                newNotfix   "not" 
            
        let newInfix: string -> int -> unit =
        fun sym prior -> 
        let tok = 
              match prior with
              | 0 -> INFIX0 sym
              | 1 -> INFIX1 sym
              | 2 -> INFIX2 sym
              | 3 -> INFIX3 sym
              | 4 -> INFIX4 sym
              | 5 -> INFIX5 sym
              | 6 -> INFIX6 sym
              | 7 -> INFIX7 sym
              | 8 -> INFIX8 sym
              | _ -> INFIX7 sym
              
        in  Hashtbl.remove symbols sym;
            Hashtbl.add    symbols sym tok
            
            
        let prior: string -> int =
        fun sym ->
            try
            match Hashtbl.find symbols sym with
            | INFIX0 _ -> 0
            | INFIX1 _ -> 1
            | INFIX2 _ -> 2
            | INFIX3 _ -> 3
            | INFIX4 _ -> 4
            | INFIX5 _ -> 5
            | INFIX6 _ -> 6
            | INFIX7 _ -> 7
            | INFIX8 _ -> 8
            | PREFIX _ -> -1 (* lower than all infixes *)
            | NOTFIX _ -> -1 (* lower than all infixes *)
            | _        -> 99
            with Not_found -> 99
            
        let bracketleftwards opl opr =  (* (a opl b) opr c *)
        let pr = prior opr
        in  pr <> 8 && pr >= prior opl
        
        let bracketrightwards opl opr = (* a opl (b opr c) *)
        let pl = prior opl
        in  pl = 8 || pl > prior opr
            
        let isinfix: string -> bool =
        fun sym ->
            try
            match Hashtbl.find symbols sym with 
                  ATOM   _ -> false 
            |     PREFIX _ -> false
            |     NOTFIX _ -> false
            |      _       -> true
            with Not_found -> false
            
        let isprefix: string -> bool =
        fun sym ->
            try
            match Hashtbl.find symbols sym with 
                  PREFIX _ -> true 
            |     NOTFIX _ -> true
            |     _ -> false
            with Not_found -> false
            
        let lookupinfix: string -> token =
        fun sym -> 
        try Hashtbl.find symbols sym with 
            Not_found -> entersymbol sym (INFIX8 sym)
        
        let lookupatom: string -> token =
        fun sym -> 
        try Hashtbl.find symbols sym with 
            Not_found -> entersymbol sym (ATOM sym)
            
        let linenum     = ref 1
        let currentpath = ref ""
        let position()  = string_of_int(!linenum)^" in "^ !currentpath
        
        let paths     = ref []
        let file path = 
            paths := (!currentpath, !linenum)::(!paths);
            currentpath := path;
            linenum := 1
            
        let endfile() = 
        match !paths with
        | []            -> ()
        | (p, ln)::rest -> currentpath:=p;
                           linenum := ln;
                           paths := rest
        
}

let  digit      = ['0'-'9']
let  letter     = ['a'-'z']
let  LETTER     = ['A'-'Z']
let  AlphaNum   = LETTER | letter | digit | '_' | '\''
let  NAME       = LETTER AlphaNum*
let  ATOM       = letter AlphaNum*
let  NONQUOTE   = [^ '"' ] 
let  INF        = ['!' '^' '%' '$' '@' '&' '/' '\\' ';' '=' '>' '<' ':' '+' '-' '*' '/' '~' '|']

rule comment = parse
        "*/"                     {token lexbuf}
|       '\n'                     {incr linenum; comment lexbuf}
|       _                        {comment lexbuf}
|       eof                      {failwith "End of file inside a comment" }

and linecomment = parse
        '\n'                     {incr linenum; token lexbuf}
|       _                        {linecomment lexbuf}
|       eof                      {failwith "End of file inside a comment" }

and  token = parse
        [' ' '\t']               {token lexbuf}
|       '\n'                     {incr linenum; token lexbuf}
|       "/**/"                   {token lexbuf}
|       "--"                     {linecomment lexbuf}
|       "/*"                     {comment lexbuf}
|       '-'?digit+               {NUM(Lexing.lexeme lexbuf)}
|       ":-"                     {IF}
|       INF INF*                 {lookupinfix(Lexing.lexeme lexbuf)}
|       '?'                      {QUERY}
|       '('                      {BRA}
|       ')'                      {KET}
|       '{'                      {CBRA}
|       '}'                      {CKET}
|       '['                      {SBRA}
|       ']'                      {SKET}
|       "[|"                     {WBRA}
|       "|]"                     {WKET}
|       ','                      {COMMA}
|       '.'                      {DOT}
|       '#' (ATOM | NAME)        {DIRECTIVE(Lexing.lexeme lexbuf)}
|       '"' NONQUOTE* '"'        {mkstring(Lexing.lexeme lexbuf)}
|       ATOM                     {lookupatom(Lexing.lexeme lexbuf)}
|       NAME                     {NAME(Lexing.lexeme lexbuf)}
|       eof                      {EOF}
|       _                        {token lexbuf}































