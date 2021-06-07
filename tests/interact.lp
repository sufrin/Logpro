repeat :- .
repeat :- repeat.

go     :- repeat, write("> "), read(L), eval(L, T), show(T), fail.


