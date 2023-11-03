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

replace(Index, Element, List, Result) :-
    nth0(Index, List, _, R),
    nth0(Index, Result, Element, R).





