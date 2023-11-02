:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).


validate_move(GameState, Col1-Row1, Col2-Row2):-
    [Board, Player, _] = GameState,
    write('1 \n'),
    in_bounds(Board, Col1-Row1), in_bounds(Board, Col2-Row2),
    write('2 \n'),
    position(Board, Col1-Row1, Piece1), position(Board, Col2-Row2, Piece2),
    write('3 \n'),
    \+piece_info(Piece1, neutral), piece_info(Piece2, neutral),
    write('4 \n'),
    piece_info(Piece, Player, Piece1),
    write('5 \n'),
    write(Piece),
    valid_direction(Piece, Col1-Row1, Col2-Row2),
    write('6 \n'),
    \+path_obstructed(Board, Col1-Row1, Col2-Row2),
    write('7 \n').

valid_direction(whiteP, Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 + 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 - 1).

valid_direction(blackP, Col1-Row1, Col2-Row2):-
    (Col2 =:= Col1 + 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 + 1, Row2 =:= Row1 - 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 + 1;
    Col2 =:= Col1 - 1, Row2 =:= Row1 - 1).

move_direction(DeltaCol-DeltaRow,-1,-1) :-  
    (DeltaCol < 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,1,-1) :-  
    (DeltaCol > 0, DeltaRow < 0), !.
move_direction(DeltaCol-DeltaRow,-1,1) :-  
    (DeltaCol < 0, DeltaRow > 0), !.
move_direction(DeltaCol-DeltaRow,1,1) :-    
    (DeltaCol > 0, DeltaRow > 0), !.

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

winner_moves(Moves, WinnerMoves):-
    Moves mod 2 =:= 1,
    WinnerMoves is (Moves // 2) + 1, !.
winner_moves(Moves, WinnerMoves):-
     WinnerMoves is Moves // 2.

show_winner([_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

/*
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState).
    show_winner(GameState, Winner).
*/
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

print_turn([_,Player,_]):-
    name_of(Player, Name),
    format('~a\'s turn!\n', [Name]).

display_game([Board,_,_]) :-
    clear_console,
    display_header(1, 17),
    display_bar(17),
    display_rows(Board,1).

move(GameState, Col1-Row1-Col2-Row2, NewGameState):-
    [Board, Player, TotalMoves] = GameState,
    position(Board, Col1-Row1, Piece),
    put_piece(Board, Col1-Row1, empty, Board1),
    put_piece(Board1, Col2-Row2, Piece, Board2),
    other_player(Player, OtherPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [Board2, OtherPlayer, NewTotalMoves].


choose_move([Board, Player, TotalMoves], Col1-Row1-Col2-Row2):-
    \+difficulty(Player, _),
    repeat,
    get_move(Board, Col1-Row1-Col2-Row2),
    validate_move([Board, Player, _], Col1-Row1, Col2-Row2), !.

check_directions(Board,Player,Result):-
    findall(1,( piece_info(Type1,Player,Piece1), 
                in_bounds(Board,ColI-RowI),
                position(Board,ColI-RowI,Piece1),
                empty(ColF-RowF),
                position(Board,ColF-RowF,empty), % Water position is not filled by another piece
                valid_direction(Type1,ColI-RowI,ColF-RowF), 
                \+path_obstructed(Board,ColI-RowI,ColF-RowF)),
            List),
    length(List, Result).

choose_move([Board, Player, TotalMoves], Move):-
    difficulty(Player, Level),
    choose_move([Board, Player, TotalMoves], Player, Level, Move), !.

choose_move(GameState, Player, 1 , Col1-Row1-Col2-Row2):-
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

play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.