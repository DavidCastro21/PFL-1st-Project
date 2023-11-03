:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

put_piece(Board, Col-Row, empty, NewBoard) :-
    winBlack(Col-Row), !,
    put_piece(Board, Col-Row, winBlack, NewBoard).

put_piece(Board, Col-Row, empty, NewBoard) :-
    winWhite(Col-Row), !,
    put_piece(Board, Col-Row, winWhite, NewBoard).

put_piece(Board, Col-Row, Piece, NewBoard):-
    RowIndex is Row-1, ColIndex is Col-1,
    nth0(RowIndex, Board, Line),
    replace(ColIndex, Piece, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).

position(Board, Col-Row, Piece):-
    \+winWhite(Col-Row),
    \+winBlack(Col-Row),
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece), !.


position(Board, Col-Row, Piece) :-
    (winBlack(Col-Row); winWhite(Col-Row)),
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece),
    Piece \= empty, Piece \= winBlack, Piece \= winWhite, !.

position(_, Col-Row, winBlack):- 
    winBlack(Col-Row), !.

position(_, Col-Row, winWhite):-
    winWhite(Col-Row), !.

in_bounds(Board, Col-Row) :-
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
    format('~d  ', [N]),
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


end_board:-
    asserta((winWhite(5-1))),
    asserta((winWhite(7-1))),
    asserta((winWhite(9-1))),
    asserta((winWhite(11-1))),
    asserta((winWhite(13-1))),
    asserta((winBlack(5-9))),
    asserta((winBlack(7-9))),
    asserta((winBlack(9-9))),
    asserta((winBlack(11-9))),
    asserta((winBlack(13-9))),
    asserta((points_to_win(1))), !.

init_state(Board) :-
    board(Board),
    end_board.












 


 



 