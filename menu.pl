

draw_initial :-
    write(' ______________________________________________ '), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|         1                Play                |'), nl,
    write('|         2                Settings            |'), nl,
    write('|         3                About               |'), nl,
    write('|                                              |'), nl,
    write('|         4                Quit                |'), nl,
    write('|                                              |'), nl,
    write(' ______________________________________________ '), nl, nl,
    write('Choose an option: ').


choose_initial_option(Option) :-
    read(Option),
    Option > 0,
    Option < 5.

initial_option(1) :- 
    display_gmenu,
    choose_gmenu_option(Option),
    gmenu_option(Option).

initial_option(2) :-
    display_smenu,
    choose_smenu_option(Option),
    smenu_option(Option).

initial_option(3) :-
    display_about,
    choose_about_option(Option),
    about_option(Option).

initial_option(4) :-
    halt.



display_gmenu :-
    write(' ______________________________________________ '), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|         1                Human vs Human      |'), nl,
    write('|         2                Human vs COM        |'), nl,
    write('|         3                COM vs COM          |'), nl,
    write('|         4                Return              |'), nl,
    write('|                                              |'), nl,
    write(' ______________________________________________ '), nl, nl,
    write('Choose an option: ').

choose_gmenu_option(Option) :-
    read(Option),
    Option > 0,
    Option < 5.

gmenu_option(1) :-
    halt.

gmenu_option(2) :-
   halt.

gmenu_option(3) :-
    halt.

gmenu_option(4) :-
    draw_initial,
    choose_initial_option(Option),
    initial_option(Option).

display_smenu :-
    write(' ______________________________________________ '), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|         1                Difficulty          |'), nl,
    write('|         2                Return              |'), nl,
    write('|                                              |'), nl,
    write(' ______________________________________________ '), nl, nl,
    write('Choose an option: ').

choose_smenu_option(Option) :-
    read(Option),
    Option > 0,
    Option < 3.

smenu_option(1) :-
    draw_difmenu,
    choose_difmenu_option(Option),
    difmenu_option(Option).

smenu_option(2) :-
    draw_initial,
    choose_initial_option(Option),
    initial_option(Option).

display_difmenu :-
    write(' ______________________________________________ '), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|         1                Easy                |'), nl,
    write('|         2                Medium              |'), nl,
    write('|         3                Hard                |'), nl,
    write('|         4                Return              |'), nl,
    write('|                                              |'), nl,
    write(' ______________________________________________ '), nl, nl,
    write('Choose an option: ').

choose_difmenu_option(Option) :-
    read(Option),
    Option > 0,
    Option < 5.

difmenu_option(1) :-
    draw_smenu,
    choose_smenu_option(Option),
    smenu_option(Option).

difmenu_option(2) :-
    draw_smenu,
    choose_smenu_option(Option),  
    smenu_option(Option).

difmenu_option(3) :- 
    draw_smenu,
    choose_smenu_option(Option),
    smenu_option(Option).

difmenu_option(4) :-
    draw_smenu,
    choose_smenu_option(Option),
    smenu_option(Option).

display_about :-
    write(' ______________________________________________ '), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write('|                                              |'), nl,
    write(' ______________________________________________ '), nl, nl,
    write('Press 1 to return: ').

choose_about_option(Option) :-
    read(Option),
    Option = 1.

about_option(1) :-
    draw_initial,
    choose_initial_option(Option),
    initial_option(Option).
