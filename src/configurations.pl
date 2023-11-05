:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

choose_difficulty(Bot) :-
    format('Please select ~a difficulty:\n', [Bot]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).

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

option(1):-
    write('Human vs. Human\n'),
    get_name(player1), nl,
    get_name(player2).

option(2):-
    write('Human vs. Bot\n'),
    get_name(player1),
    asserta((name_of(player2, 'bot'))), !, 
    choose_difficulty(player2).

option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a with Black pieces\n2 - ~a with White pieces\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).

differo :-
    write('========================\n'),
    write('   Welcome to DIFFERO!  \n'),
    write('========================\n').

menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n').

print_turn([_,Player,_]):-
    name_of(Player, Name),
    format('~a\'s turn!\n', [Name]).
    
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).

configurations([Board,Player,1]):-
    differo,
    set_mode,
    init_random_state,
    choose_player(Player),
    initial_state(Board).