% name_of(+Player, -Name)
% Gets the name of a player
:- dynamic name_of/2.

% difficulty(+Player, -Difficulty)
% Gets the bot difficulty
:- dynamic difficulty/2.

% nonblock(+Coordinate)
% Find out nonblock coordinates
:- dynamic nonblock/1.

% winBlack(+Coordinate)
% Find out winBlack coordinates
:- dynamic winBlack/1.

% winWhite(+Coordinate)
% Find out winWhite coordinates
:- dynamic winWhite/1.

% board(-Board)
% Gets the board
board([
        [nonblock,     nonblock,      nonblock,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     empty,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     empty,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      empty,     nonblock,     black,     nonblock,     empty,     nonblock,     black,     nonblock,     empty,     nonblock,     black,     nonblock,     empty,     nonblock,     nonblock],
        [nonblock,     empty,      nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     black,     nonblock,     empty,     nonblock],
        [empty,     nonblock,      empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty],
        [nonblock,     empty,      nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     empty,     nonblock],
        [nonblock,     nonblock,      empty,     nonblock,     white,     nonblock,     empty,     nonblock,     white,     nonblock,     empty,     nonblock,     white,     nonblock,     empty,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     empty,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     white,     nonblock,     empty,     nonblock,     nonblock,     nonblock],
        [nonblock,     nonblock,      nonblock,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     empty,     nonblock,     nonblock,     nonblock,     nonblock]
]).


% piece_info(+Piece, +Player, -Info)
% Helps to know the information of a piece
piece_info(whiteP, player2, white).
piece_info(blackP, player1, black).
piece_info(winBlack, neutral).
piece_info(winWhite, neutral).
piece_info(nonblock, nonblock).
piece_info(empty, neutral).

% other_player(+Player, -OtherPlayer)
% Change the player turn
other_player(player1, player2).
other_player(player2, player1).

% symbol(+Piece, -Symbol)
% Attribute a symbol to a piece to be displayed on the board
symbol(winBlack, 'o') :- !.
symbol(winWhite, 'O') :- !.
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(nonblock , '#') :- !.
symbol(empty,' ') :- !.