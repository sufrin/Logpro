## LogPro -- an open Prolog implementation

LogPro was designed to be a flexible logic programming engine. I
used it at Oxford around the turn of the millenium in support of a
Programming Language Principles course.  The idea was to animate
operational semantics and type inference by tracing the inference
steps.

There is a manual and some introductory lecture notes on 
the language. The notes include a discursive account of a 
term rewriting system. 

### Building the system

        cd   src
        make logpro
 
The tests directory has a handful of examples of small-step and
evaluation semantics; as well as a small term-rewriting engine and a 
polymorphic type system (sort of). A lambda-calculus operational 
semantics demonstrates multiple reduction paths

        cd ../tests
        ../src/logpro lambdared.lp
        -- prob2?
        let x = 1 in x + x + let y = 4 in y + 4 -->
        {x => x + x}[1] + let y = 4 in y + 4 -->
        (1 + 1) + let y = 4 in y + 4 -->
        2 + let y = 4 in y + 4 -->
        2 + {y => y + 4}[4] -->
        2 + 4 + 4 -->
        2 + 8 -->
        10
        yes?
       (1 + 1) + let y = 4 in y + 4 -->
       (1 + 1) + {y => y + 4}[4] -->
       2 + {y => y + 4}[4] -->
       2 + 4 + 4 -->
       2 + 8 -->
       10
       yes ?
       
etc. etc.

The interpreter compiled and ran the tests successfully this morning
with Ocaml 4.11.  This was nearly 20 years after it last compiled: 
Xavier Leroy and his colleagues are to be congratulated for the
superb quality and consistency of the Ocaml language definition and implementation
that made this possible.


Bernard Sufrin, Oxford, June 2021.


