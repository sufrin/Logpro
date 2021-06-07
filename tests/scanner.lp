/*
        $Id: scanner.lp,v 1.1 2000/01/12 10:24:00 sufrin Exp $
*/
#use lib
symbol(S, R, S')  :- cat(" ", T, S), symbol(T, R, S').
symbol(S, R, S')  :- hd(S, Letter), 
                     Letter/=" ", 
                     tl(S, S''), 
                     symbody(S'', Letter, R, S').
symbody(S, R, R,  S)  :- hd(S, " ").
symbody(S, R, Res, S') :- show(S, R, S'),
                     hd(S, Letter), Letter/=" ", 
                     tl(S, S''), 
                     cat(R, Letter, R'),
                     symbody(S'', R', Res, S').


