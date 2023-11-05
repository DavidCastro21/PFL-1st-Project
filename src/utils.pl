:- use_module(library(between)).


clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

clear_console:- 
    write('\33\[2J').

init_random_state :-
    now(X),
    setrand(X).

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

display_game([Board,_,_]) :-
    clear_console,
    display_header(1, 17),
    display_bar(17),
    display_rows(Board,1).

clear_data :-
    retractall(winBlack(_)),
    retractall(winWhite(_)),
    retractall(difficulty(_,_)),
    retractall(name_of(_,_)).

get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).

get_line(Result, Acc):-
    atom_chars(Result, Acc).

get_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    get_line(Name, []),
    asserta(name_of(Player, Name)).

read_number(X):-
    read_number_aux(X,0).

read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).

read_number_aux(X,X).

get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

get_move(Board, Col1-Row1-Col2-Row2):-
    get_option(1, 17, 'Column of the piece to move', Col1),
    get_option(1, 9, 'Row of the piece to move', Row1),
    get_option(1, 17, 'Column of the destination', Col2),
    get_option(1, 9, 'Row of the destination', Row2).

show_winner([_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

replace(Index, Element, List, Result) :-
    nth0(Index, List, _, R),
    nth0(Index, Result, Element, R).

swap_minimax(min, max).
swap_minimax(max, min).

eval(min, [Value|_], Result):- Result is -Value.
eval(max, Values, Value):- last(Values, Value).


