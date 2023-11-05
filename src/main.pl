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
    get_move_choice(GameState, Choice),
    process_choice(GameState, Choice).

get_move_choice([Board, Player, TotalMoves], Choice):-
    \+difficulty(Player, _),
    write('Choose a move:\n'),
    write('1 - Move\n'),
    write('2 - Eat\n'),
    get_option(1, 2, 'Choice', Choice), !.

get_move_choice([Board, Player, TotalMoves], Choice):-
    difficulty(Player, Level),
    Choice = 1.

process_choice(GameState, 1):-
    choose_move(GameState, Col1-Row1-Col2-Row2),
    move(GameState, Col1-Row1-Col2-Row2, NewGameState),
    game_cycle(NewGameState).


process_choice(GameState, 2):-
    choose_eat(GameState, Col1-Row1-Col2-Row2),
    move_eat(GameState, Col1-Row1-Col2-Row2, NewGameState),
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


game_over([Board, Player, _], Winner):-
    points_to_win(WinnerPoints),
    other_player(Player, Winner),
    count_end_positions(Board, Winner, WinnerPoints),
    name_of(Player, Name),
    format('~a\'s WIN!\n', [Name]).


count_end_positions(Board, Winner, WinnerPoints):-
    findall(1, (winBlack(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    length(End, WinnerPoints).

count_end_positions(Board, Winner, WinnerPoints):-
    findall(1, (winWhite(Coordinate), piece_info(_, Player, Piece), position(Board, Coordinate, Piece)), End),
    length(End, WinnerPoints).


value([Board, OtherPlayer, _], Player, Value) :-
    check_directions(Board, Player, EndsReachable),
    Value is 100 * EndsReachable.

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