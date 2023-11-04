:- dynamic name_of/2.

:- dynamic difficulty/2.

:- dynamic nonblock/1.

:- dynamic winBlack/1.

:- dynamic winWhite/1.


board([
        [nonblock,     nonblock,      nonblock,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     empty,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     empty,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      empty,     nonblock,     black,     nonblock,     empty,     nonblock,     black,     nonblock,     empty,     nonblock,     black,     nonblock,     empty,     nonblock,     nonblock],
        [nonblock,     empty,      nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     empty,     nonblock],
        [empty,     nonblock,      empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty],
        [nonblock,     empty,      nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     empty,     nonblock],
        [nonblock,     nonblock,      empty,     nonblock,     white,     nonblock,     empty,     nonblock,     white,     nonblock,     empty,     nonblock,     white,     nonblock,     empty,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     empty,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     empty,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     nonblock,     nonblock,     nonblock]
]).



piece_info(whiteP, player2, white).
piece_info(blackP, player1, black).
piece_info(winBlack, neutral).
piece_info(winWhite, neutral).
piece_info(nonblock, nonblock).
piece_info(empty, neutral).

other_player(player1, player2).
other_player(player2, player1).


symbol(winBlack, 'o') :- !.
symbol(winWhite, 'O') :- !.
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(nonblock , '#') :- !.
symbol(empty,' ') :- !.