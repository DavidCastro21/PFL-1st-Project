:- use_module(library(between)).


% clear_buffer/0
% Clears the buffer
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

% clear_console/0
% Clears the console
clear_console:- 
    write('\33\[2J').

% init_random_state/0
% Initializes the random state
init_random_state :-
    now(X),
    setrand(X).

% display_bar(+N)
% Displays a bar with N dashes
display_bar(0) :-
    write('|\n'), !.
display_bar(N) :-
    write('|---'),
    N1 is N-1,
    display_bar(N1).

% display_header(+N, +Max)
% Displays the header of the board
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

% get_symbol(+Board, +Line, +Col, -Symbol)
% Gets the symbol of a piece on the Col and Line coordinates of the board
get_symbol(Board, Line, Col, Symbol) :-
    position(Board, Col-Line, Piece),
    symbol(Piece, Symbol).

% display_pieces(+Board, +Line, +Col)
% Displays the pieces of the board
display_pieces(_,_,Col):-
    Col > 17, write('\n  '), !.
display_pieces(Board, Line, Col) :-
    get_symbol(Board, Line, Col, Symbol),
    format(' ~a |', [Symbol]),
    Col1 is Col+1,
    display_pieces(Board, Line, Col1).

% display_rows(+Board, +Line)
% Displays the rows of the board
display_rows(_, Line) :-
    Line > 9 ,nl, !.
display_rows(Board, Line) :-
    format('~d |', [Line]),
    display_pieces(Board, Line, 1),
    display_bar(17),
    Line1 is Line+1,
    display_rows(Board, Line1).

% display_game(+GameState)
% Displays the game board
display_game([Board,_,_]) :-
    clear_console,
    display_header(1, 17),
    display_bar(17),
    display_rows(Board,1).

% clear_data/0
% Clears the data from the previous game
clear_data :-
    retractall(winBlack(_)),
    retractall(winWhite(_)),
    retractall(difficulty(_,_)),
    retractall(name_of(_,_)).

% get_line(-Result, +Acc)
% Reads a line from the input and returns it on Result
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Result, Acc):-
    atom_chars(Result, Acc).

% get_name(+Player)
% Asks the user to choose a name
get_name(Player):-
    format('Hello ~a, what is your name? ', [Player]),
    get_line(Name, []),
    asserta(name_of(Player, Name)).

% read_number(-X)
% Reads a number from the input
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

% get_option(+Min, +Max, +Context, -Value)
% Asks the user to choose an option between Min and Max
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% get_move(+Board, -Move)
% Asks the user to choose a move
get_move(Board, Col1-Row1-Col2-Row2):-
    get_option(1, 17, 'Column of the piece to move', Col1),
    get_option(1, 9, 'Row of the piece to move', Row1),
    get_option(1, 17, 'Column of the destination', Col2),
    get_option(1, 9, 'Row of the destination', Row2).

% winner_moves(+Moves, -WinnerMoves)
% Calculates the number of moves of the winner
winner_moves(Moves, WinnerMoves):-
    Moves mod 2 =:= 1,
    WinnerMoves is (Moves // 2) + 1, !.
winner_moves(Moves, WinnerMoves):-
    WinnerMoves is Moves // 2.

% show_winner(+GameState, +Winner)
% Displays the winner of the game
show_winner([_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

% replace(+Index, +Element, +List, -Result)
% Return on Result the list with the element replaced
replace(Index, Element, List, Result) :-
    nth0(Index, List, _, R),
    nth0(Index, Result, Element, R).

% swap_minimax(+Player, -OtherPlayer)
% Swaps minimax algorithm code
swap_minimax(min, max).
swap_minimax(max, min).

% eval(+Player, +Values, -Result)
% Evaluates the values according to the minimax algorithm
eval(min, [Value|_], Result):- Result is -Value.
eval(max, Values, Value):- last(Values, Value).


