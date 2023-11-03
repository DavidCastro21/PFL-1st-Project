:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).


choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).


option(1):-
    write('Human vs. Human\n'),
    get_name(player1), get_name(player2).
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
    format('Who are the starting and black pieces?\n1 - ~a with UPPERCASE animals\n2 - ~a with lowercase animals\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).

differo :-
    write('========================\n'),
    write('  Welcome to DIFFERO!\n'),
    write('========================\n').


menu:-  
    write('Please select game mode:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Bot\n'),
    write('3 - Bot vs. Bot\n').


set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).


configurations([Board,Player,1]):-
    differo,
    set_mode,
    init_random_state,
    choose_player(Player),
    init_state(Board).