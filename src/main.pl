:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).


game_cycle(GameState):-
    /*game_over(GameState, Winner), !,*/
    display_game(GameState).
    /*show_winner(GameState, Winner).*/
game_cycle(GameState):-
    display_game(GameState).
    /*
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).*/

display_game([Board,_,_]) :-
    clear_console,
    display_header(1, 17),
    display_bar(17),
    display_rows(Board,1).

play :-
    configurations(GameState), !,
    game_cycle(GameState).
    %clear_data.