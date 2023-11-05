:- use_module(library(lists)).
:- consult(data).
:- consult(utils).


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
    \+winBlack(Col-Row),
    \+winWhite(Col-Row),
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece), !.

validate_move(GameState, Col1-Row1, Col2-Row2):-
    [Board, Player, _] = GameState,
    in_bounds(Board, Col1-Row1), in_bounds(Board, Col2-Row2),
    position(Board, Col1-Row1, Piece1), position(Board, Col2-Row2, Piece2),
    \+piece_info(Piece1, neutral), piece_info(Piece2, neutral),
    piece_info(Piece, Player, Piece1),
    valid_direction(Piece, Col1-Row1, Col2-Row2),
    \+path_obstructed(Board, Col1-Row1, Col2-Row2),
    (
        (Player = player1, Piece2 \= winWhite);
        (Player = player2, Piece2 \= winBlack)
    ).

validate_eat_move(GameState, Col1-Row1, Col2-Row2):-
    [Board, Player, _] = GameState,
    in_bounds(Board, Col1-Row1), in_bounds(Board, Col2-Row2),
    position(Board, Col1-Row1, Piece1), position(Board, Col2-Row2, Piece2),
    piece_info(Piece2, neutral),
    valid_eat(Piece, Col1-Row1, Col2-Row2).

valid_eat(whiteP,Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 2, Row2 =:= Row1 + 2;
    Col2 =:= Col1 + 2, Row2 =:= Row1 - 2;
    Col2 =:= Col1 - 2, Row2 =:= Row1 + 2;
    Col2 =:= Col1 - 2, Row2 =:= Row1 - 2).

valid_eat(blackP,Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 2, Row2 =:= Row1 + 2;
    Col2 =:= Col1 + 2, Row2 =:= Row1 - 2;
    Col2 =:= Col1 - 2, Row2 =:= Row1 + 2;
    Col2 =:= Col1 - 2, Row2 =:= Row1 - 2).

valid_direction(whiteP, Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 + 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 + 2, Row2 =:= Row1;
    Col2 =:= Col1 - 2, Row2 =:= Row1).

valid_direction(blackP, Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 + 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 + 2, Row2 =:= Row1;
    Col2 =:= Col1 - 2, Row2 =:= Row1).

move_direction(DeltaCol-DeltaRow,-1,-1) :-  
    (DeltaCol < 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,1,-1) :-  
    (DeltaCol > 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,-1,1) :-  
    (DeltaCol < 0, DeltaRow > 0), !.
move_direction(DeltaCol-DeltaRow,1,1) :-    
    (DeltaCol > 0, DeltaRow > 0), !.
move_direction(DeltaCol-DeltaRow,2,0) :-
    (DeltaCol > 1, DeltaRow =:= 0), !.
move_direction(DeltaCol-DeltaRow,-2,0) :-
    (DeltaCol < -1, DeltaRow =:= 0), !.

path_obstructed(Board, Col1-Row1, Col2-Row2):-
    DeltaCol is Col2-Col1, DeltaRow is Row2-Row1,
    move_direction(DeltaCol-DeltaRow, HorDir, VerDir),
    \+path_obstructedAux(Board, Col1-Row1, Col2-Row2, HorDir-VerDir).

path_obstructedAux(_, Col-Row, Col-Row, _):- !.
path_obstructedAux(Board, Col1-Row1, Col2-Row2, Hordir-Verdir):-
    Col3 is Col1+Hordir, Row3 is Row1+Verdir,
    position(Board, Col3-Row3, Piece),
    piece_info(Piece, neutral), !,
    path_obstructedAux(Board, Col3-Row3, Col2-Row2, Hordir-Verdir).

move(GameState, Col1-Row1-Col2-Row2, NewGameState):-
    [Board, Player, TotalMoves] = GameState,
    position(Board, Col1-Row1, Piece),
    put_piece(Board, Col1-Row1, empty, Board1),
    put_piece(Board1, Col2-Row2, Piece, Board2),
    other_player(Player, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [Board2, OtherPlayer, NewTotalMoves].

move_eat(GameState, Col1-Row1-Col2-Row2, NewGameState):-
    [Board, Player, TotalMoves] = GameState,
    position(Board, Col1-Row1, Piece),
    put_piece(Board, Col1-Row1, empty, Board1),
    put_piece(Board1, Col2-Row2, Piece, Board2),
    Col3 is Col2 + Col1, Row3 is Row2 + Row1,
    ColF is Col3 // 2, RowF is Row3 // 2,
    put_piece(Board2, ColF-RowF, empty, Board3),
    other_player(Player, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [Board3, OtherPlayer, NewTotalMoves].
    
valid_moves(GameState, _, ListOfMoves):-
    findall(Col1-Row1-Col2-Row2, validate_move(GameState, Col1-Row1, Col2-Row2), ListOfMoves),
    \+length(ListOfMoves, 0).

valid_moves(GameState, Player, ListOfMoves) :-
    [Board, _, TotalMoves] = GameState,
    findall(Col1-Row1-Col2-Row2, validate_move([Board, Player, TotalMoves], Col1-Row1, Col2-Row2), ListOfMoves).

count_end_positions(Board, Winner, WinnerPoints):-
    findall(1, (winBlack(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    length(End, WinnerPoints).

count_end_positions(Board, Winner, WinnerPoints):-
    findall(1, (winWhite(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    length(End, WinnerPoints).

value([Board, OtherPlayer, _], Player, Value) :-
    count_end_positions(Board, Player, WinnerPoints),
    check_directions(Board, Player, EndsReachable),
    Value is 1000 * WinnerPoints + EndsReachable. 

check_directions(Board,Player,Result):-
    findall(1,( piece_info(Type1,Player,Piece1), 
                in_bounds(Board,ColI-RowI),
                position(Board,ColI-RowI,Piece1),
                piece_info(winBlack, _, Piece2),
                position(Board,ColF-RowF,empty), 
                valid_direction(Type1,ColI-RowI,ColF-RowF), 
                \+path_obstructed(Board,ColI-RowI,ColF-RowF)),
            List),
    length(List, Result).

check_directions(Board,Player,Result):-
    findall(1,( piece_info(Type1,Player,Piece1), 
                in_bounds(Board,ColI-RowI),
                position(Board,ColI-RowI,Piece1),
                piece_info(winWhite, _, Piece2),
                position(Board,ColF-RowF,empty), 
                valid_direction(Type1,ColI-RowI,ColF-RowF), 
                \+path_obstructed(Board,ColI-RowI,ColF-RowF)),
            List),
    length(List, Result).

choose_move([Board, Player, TotalMoves], Col1-Row1-Col2-Row2):-
    \+difficulty(Player, _),
    repeat,
    get_move(Board, Col1-Row1-Col2-Row2),
    validate_move([Board, Player, TotalMoves],Col1-Row1, Col2-Row2), !.

choose_move([Board, Player, TotalMoves], Move):-
    difficulty(Player, Level),
    choose_move([Board, Player, TotalMoves], Player, Level, Move), !.

choose_move(GameState, Player, 1, ColI-RowI-ColF-RowF):-
    valid_moves(GameState, Player, ListOfMoves),
    random_member(ColI-RowI-ColF-RowF, ListOfMoves).


choose_move(GameState, Player, 2, ColI-RowI-ColF-RowF):-
	valid_moves(GameState, Player, ListOfMoves),
    other_player(Player, NewPlayer),
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState), 
                                value(NewGameState,Player, Value1),
                                minimax(NewGameState, NewPlayer, min, 1, Value2),
                                Value is Value1 + Value2), Pairs),
    sort(Pairs, SortedPairs),
    last(SortedPairs, Max-_),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates).

choose_eat([Boar,Player,TotalMoves], Col1-Row1-Col2-Row2):-
    \+difficulty(Player, _),
    repeat,
    GameState = [Board, Player, TotalMoves],
    get_move(Board, Col1-Row1-Col2-Row2),
    validate_eat_move(GameState, ColI-RowI, ColF-RowF), !.

minimax(_, _, _, 1, 0):- !.
minimax(GameState, Player, Type, Level, Value):-
	other_player(Player, NewPlayer),
	swap_minimax(Type, NewType),
    NextLevel is Level + 1,
	valid_moves(GameState, Player, ListOfMoves),
	setof(Val, (  member(Coordinate, ListOfMoves), 
                  move(GameState, Coordinate, NewGameState), 
                  value(NewGameState,Player,Value1),
                  minimax(NewGameState, NewPlayer, NewType, NextLevel, Value2), 
                  Val is Value1 + Value2), Values),
    eval(Type, Values, Value).

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

initial_state(Board) :-
    board(Board),
    end_board.












 


 



 