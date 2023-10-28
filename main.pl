%% Load librarys and other code files
:- use_module(library(lists)).
:- use_module(library(random)).
:- [menu, board].

play :-
    draw_initial,
    choose_initial_option(Option), nl,
    initial_option(Option),
    fail.