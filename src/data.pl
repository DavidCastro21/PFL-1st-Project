:- dynamic name_of/2.

:- dynamic difficulty/2.

:- dynamic nonblock/2.

board([
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty]
]).


symbol(nonblock , 'X') :- !.
symbol(empty,' ') :- !.