:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).


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

winner_moves(Moves, WinnerMoves):-
    Moves mod 2 =:= 1,
    WinnerMoves is (Moves // 2) + 1, !.
winner_moves(Moves, WinnerMoves):-
     WinnerMoves is Moves // 2.

show_winner([_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).


game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).

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

valid_moves(GameState, _, ListOfMoves):-
    write ('1 \n'),
    findall(Col1-Row1-Col2-Row2, validate_move(GameState, Col1-Row1, Col2-Row2), ListOfMoves),
    write('2 \n'),
    \+length(ListOfMoves, 0),
    write('3  \n').

valid_moves(GameState, Player, ListOfMoves) :-
    [Board, _, TotalMoves] = GameState,
    findall(Col1-Row1-Col2-Row2, validate_move([Board, Player, TotalMoves], Col1-Row1, Col2-Row2), ListOfMoves).


game_over([Board, Player, _], Winner):-
    points_to_win(WinnerPoints),
    other_player(Player, Winner),
    count_end_positions(Board, Winner, WinnerPoints),
    name_of(Player, Name),
    format('~a\'s WIN!\n', [Name]).

count_end_positions(Board, Winner, WinnerPoints):-
    write('7 \n'),
    Player == player1,
    write('8 \n'),
    findall(1, (winBlack(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    write('9 \n'),
    length(End, WinnerPoints).

count_end_positions(Board, Winner, WinnerPoints):-
    write('10 \n'),
    Player == player2,
    write('11 \n'),
    findall(1, (winWhite(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    write('12 \n'),
    length(End, WinnerPoints).


value([Board, OtherPlayer, _], Player, Value) :-
    write('13 \n'),
    count_end_positions(Board, Player, winBlack),
    write('14 \n'),
    count_end_positions(Board, Player, winWhite),
    write('15 \n'),
    EndDiff is winBlack - winWhite,
    write('16 \n'),
    check_directions(Board, Player, EndsReachable),
    write('17 \n'),
    Value is 100 * EndDiff + EndsReachable,
    write('18 \n').

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
    write('19 \n'),
    valid_moves(GameState, Player, ListOfMoves),
    write('20 \n'),
    length(ListOfMoves, NumMoves),
    format('~d \n', [NumMoves]),
    write('21 \n'),
    random(0, NumMoves, RandomIndex),
    write('22 \n'),
    nth0(RandomIndex, ListOfMoves, ColI-RowI-ColF-RowF).


choose_move(GameState, Player, 2, ColI-RowI-ColF-RowF):-
    write('22 \n'),
	valid_moves(GameState, Player, ListOfMoves),
    write('23 \n'),
    other_player(Player, NewPlayer),
    write('24 \n'),
	findall(Value-Coordinate, ( member(Coordinate, ListOfMoves), 
                                move(GameState, Coordinate, NewGameState), 
                                value(NewGameState,Player, Value1),
                                minimax(NewGameState, NewPlayer, min, 1, Value2),
                                Value is Value1 + Value2), Pairs),
    write('25 \n'),
    sort(Pairs, SortedPairs),
    write('26 \n'),
    last(SortedPairs, Max-_),
    write('27 \n'),
    findall(Coordinates, member(Max-Coordinates, SortedPairs), MaxCoordinates),
    write('28 \n'),
    random_member(ColI-RowI-ColF-RowF, MaxCoordinates),
    write('29\n').

minimax(_, _, _, 2, 0):- !.
minimax(GameState, Player, Type, Level, Value):-
	other_player(Player, NewPlayer),
	swap_minimax(Type, NewType),
    NextLevel is Level + 1,
    write('30 \n'),
	valid_moves(GameState, Player, ListOfMoves),
    write('31 \n'),
	setof(Val, (  member(Coordinate, ListOfMoves), 
                  move(GameState, Coordinate, NewGameState), 
                  value(NewGameState,Player,Value1),
                  minimax(NewGameState, NewPlayer, NewType, NextLevel, Value2), 
                  Val is Value1 + Value2), Values),
                  write('32 \n'),
    eval(Type, Values, Value).


play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.