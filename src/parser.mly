%{

        (* $Id: parser.mly,v 1.28 2000/02/07 00:27:54 sufrin Exp $ *)

        open Syntax
        
%}

%token <string>  NAME 
%token <string>  ATOM
%token <string>  PREFIX NOTFIX
%token <string>  INFIX0 INFIX1 INFIX2 INFIX3 INFIX4 INFIX5 INFIX6 INFIX7 INFIX8
%right NOTFIX
%right INFIX0
%right INFIX1
%right INFIX2
%right INFIX3
%right INFIX4
%right INFIX5
%right INFIX6
%right INFIX7
%left  INFIX8
%right WBRA WKET
%left  SBRA
%right PREFIX
%token <string>  NUM
%token <string>  STRING
%token <string>  DIRECTIVE
%token BRA KET CBRA CKET SBRA SKET WBRA WKET COMMA IF DOT QUERY EOF  
%type  <Syntax.cmd>  cmd
%type  <Syntax.form> formula

%start cmd
%start formula

%%
/*
        Command language proper
*/

cmd     :       Form DOT            {Clause($1, [])}
        |       Form IF DOT         {Clause($1, [])}
        |       Form IF Forms DOT   {Clause($1, $3)}
        |       IF Forms DOT        {Query $2}
        |       Forms QUERY         {Query $1}
        |       EOF                 {Eof}
        
Forms   :       Form                {[$1]}
        |       Form COMMA Forms    {$1::$3}

Form    :       Part                {$1}
        |       Form INFIX0 Form    {Struct($2, [$1; $3])}
        |       Form INFIX1 Form    {Struct($2, [$1; $3])}
        |       Form INFIX2 Form    {Struct($2, [$1; $3])}
        |       Form INFIX3 Form    {Struct($2, [$1; $3])}
        |       Form INFIX4 Form    {Struct($2, [$1; $3])}
        |       Form INFIX5 Form    {Struct($2, [$1; $3])}
        |       Form INFIX6 Form    {Struct($2, [$1; $3])}
        |       Form INFIX7 Form    {Struct($2, [$1; $3])}
        |       Form INFIX8 Form    {Struct($2, [$1; $3])}
        |       Form SBRA Form SKET {Struct("[]", [$1; $3])}
        |       WBRA Form WKET Form {Struct("[||]", [$2; $4])}
        |       NOTFIX Form         {Struct($1, [$2])}
        |       PREFIX Form         {Struct($1, [$2])}
        |       error               {failwith "Error in formula"}

Part    :       NUM                 {Num(int_of_string $1)}
        |       STRING              {String $1}
        |       NAME                {Var(mkname $1)}
        |       BRA Form KET        {$2}
        |       Struct              {$1}
        

Struct  :       ATOM                {Struct($1, [])}
        |       ATOM BRA Forms KET  {Struct($1, $3)}
        |       ATOM BRA       KET  {Struct($1, [])}
        |       CBRA Forms CKET     {Struct("{}", $2)}
        |       CBRA       CKET     {Struct("{}", [])}
                /* quoted operators */
        |       BRA INFIX0 KET      {Struct($2, [])}
        |       BRA INFIX1 KET      {Struct($2, [])}
        |       BRA INFIX2 KET      {Struct($2, [])}
        |       BRA INFIX3 KET      {Struct($2, [])}
        |       BRA INFIX4 KET      {Struct($2, [])}
        |       BRA INFIX5 KET      {Struct($2, [])}
        |       BRA INFIX6 KET      {Struct($2, [])}
        |       BRA INFIX7 KET      {Struct($2, [])}
        |       BRA INFIX8 KET      {Struct($2, [])}
        |       BRA NOTFIX KET      {Struct($2, [])}
        |       BRA PREFIX KET      {Struct($2, [])}
        |       BRA SBRA SKET KET   {Struct("[]",   [])}
        |       BRA WBRA WKET KET   {Struct("[||]", [])}
                /* applications of quoted operators */
        |       BRA INFIX0 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX1 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX2 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX3 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX4 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX5 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX6 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX7 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA INFIX8 KET BRA Forms KET     {Struct($2, $5)}
        |       BRA NOTFIX KET BRA Forms KET     {Struct($2, $5)}
        |       BRA PREFIX KET BRA Forms KET     {Struct($2, $5)}
        |       BRA SBRA SKET KET  BRA Forms KET {Struct("[]",   $6)}
        |       BRA WBRA WKET KET  BRA Forms KET {Struct("[||]", $6)}



/* 
        Directive language 
*/
cmd     :       DIRECTIVE Thing     {Directive [$1; $2]}
        |       DIRECTIVE DOT       {Directive [$1]}
        |       DOT                 {Directive []}
        
Thing   :       NAME                {$1} 
        |       ATOM                {$1} 
        |       NUM                 {$1} 
        |       STRING              {$1} 
        |       INFIX0              {$1} 
        |       INFIX1              {$1} 
        |       INFIX2              {$1} 
        |       INFIX3              {$1} 
        |       INFIX4              {$1} 
        |       INFIX5              {$1} 
        |       INFIX6              {$1} 
        |       INFIX7              {$1} 
        |       INFIX8              {$1} 
        |       NOTFIX              {$1} 
        |       PREFIX              {$1} 
        
formula:        Form                {$1}


























