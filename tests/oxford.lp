/*
        Oxford street map
        $Id: oxford.lp,v 1.1 2000/01/14 10:29:34 sufrin Exp $
        
*/

go(n,   parks,          oucl,           thom).
go(n,   parks,          thom,           jenkin).
go(w,   keble,          oucl,           ounp).

go(s,   banbury,        jenkin,         ounp).
go(s,   banbury,        ounp,           qeh).
go(s,   banbury,        qeh,            warmemorial).

go(n,   woodstock,      warmemorial,    stannes).

go(s,   stgiles,        warmemorial,    stjohns).
go(s,   stgiles,        stjohns,        balliol).
go(e,   broad,          balliol,        trinity).
go(e,   broad,          trinity,        blackwells).
go(e,   broad,          blackwells,     newbodleian).
go(n,   parks,          newbodleian,    wadham).
go(n,   parks,          wadham,         radcliffe).
go(n,   parks,          radcliffe,      museum).
go(n,   parks,          museum,         keble).

cross(n, keble,            keble,          oucl).
cross(s, broad,            trinity,        bodleian).
cross(s, broad,            trinity,        blackwellsmusic).
cross(s, broad,            blackwells,     bodleian).

go(s,   turl,           blackwellsmusic, jesus).
go(s,   turl,           jesus,           lincoln).
go(e,   high,           lincoln,         brasenose).
go(e,   high,           brasenose,       stmary).
go(e,   high,           stmary,          allsouls).

cross(s, high, brasenose, bottoms_up).
cross(s, high, stmary,    univ).

go(s, stmichael, bottoms_up, oriel).
cross(w, stmichael, oriel, chch).

route(Start, Start, nil)                    :- .
route(Start, Dest,  (Street --Dir--> Place):Via) :- get(Dir, Street, Start, Place), 
                                               route(Place, Dest, Via).
                                               
get(Dir,   Street, Start, Place)      :- go(Dir, Street, Start, Place).
get(Dir,   Street, Start, Place)      :- go(Dir', Street, Start, Place), opp(Dir, Dir').
get(cross(Dir), Street, Start, Place) :- cross(Dir, Street, Start, Place).
                                  
opp(n, s).
opp(s, n).
opp(e, w).
opp(w, e).



