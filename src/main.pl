:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).

winner_moves(Moves, WinnerMoves):-
    Moves mod 2 =:= 1,
    WinnerMoves is (Moves // 2) + 1, !.

winner_moves(Moves, WinnerMoves):-
    WinnerMoves is Moves // 2.

game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).

game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    get_move_choice(GameState, Choice),
    process_choice(GameState, Choice).

game_over([Board, Player, _], Winner):-
    points_to_win(WinnerPoints),
    other_player(Player, Winner),
    count_end_positions(Board, Winner, WinnerPoints),
    name_of(Player, Name),
    format('~a\'s WIN!\n', [Name]).

play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.