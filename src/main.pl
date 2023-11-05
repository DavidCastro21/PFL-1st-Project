:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).

% game_cycle(+GameState)
% Loop that will run the game until it is over
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    get_move_choice(GameState, Choice),
    process_choice(GameState, Choice).

% play/0
% Starts the game and clears the data
play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.