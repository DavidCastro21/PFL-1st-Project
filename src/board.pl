:- use_module(library(lists)).
:- consult(data).
:- consult(utils).


position(Board, Col-Row, Piece) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece).

position(Board, Col-Row, Piece) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece),
    Piece \= empty, !.

in_bounds(Board, Col-Row) :-
    length(Board, 17),
    between(1, 9, Row),
    between(1, 17, Col).

display_bar(0) :-
    write('|\n'), !.

display_bar(N) :-
    write('|---'),
    N1 is N-1,
    display_bar(N1).

display_header(Max, Max) :-
    format('~d\n  ', [Max]), !.

display_header(1, Max) :-
    write('\n    1   '),
    display_header(2, Max), !.

display_header(N, Max) :-
    N > 9, 
    format('~d   ', [N]),
    N1 is N+1,
    display_header(N1, Max), !.

display_header(N, Max) :-
    format('~d   ', [N]),
    N1 is N+1,
    display_header(N1, Max).

get_symbol(Board, Line, Col, Symbol) :-
    position(Board, Col-Line, Piece),
    symbol(Piece, Symbol).

display_pieces(_,_,Col):-
    Col > 17, write('\n  '), !.

display_pieces(Board, Line, Col) :-
    get_symbol(Board, Line, Col, Symbol),
    format(' ~a |', [Symbol]),
    Col1 is Col+1,
    display_pieces(Board, Line, Col1).

display_rows(_, Line) :-
    Line > 9 ,nl, !.

display_rows(Board, Line) :-
    format('~d |', [Line]),
    display_pieces(Board, Line, 1),
    display_bar(17),
    Line1 is Line+1,
    display_rows(Board, Line1).

init_state(Board) :-
    board(Board).



 