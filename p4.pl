%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%     FACTS     %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position

/*
corner_square(1, 1).    %%% map corner squares to board squares
corner_square(2, 7).
corner_square(3, 35).
corner_square(4, 42).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%   MAIN PROGRAM   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :-
    hello,          %%% Display welcome message, initialize game

    play(1),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect 4.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
%    asserta( board([E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E]) )  %%% create a blank board
    asserta( board([[E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E]]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
%    output_winner(B),
%    retract(board(_)),
%    retract(player(_,_)),
%    read_play_again(V), !,
%    (V == 'Y' ; V == 'y'), 
%    !,
    run
    .

    read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .


read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .


%.......................................
% square
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

/*
square([M,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],1,M).
square([_,M,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],2,M).
square([_,_,M,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],3,M).
square([_,_,_,M,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],4,M).
square([_,_,_,_,M,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],5,M).
square([_,_,_,_,_,M,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],6,M).
square([_,_,_,_,_,_,M, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_, _,_,_,_,_,_,_],7,M).
*/

square([[M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],1,M).

square([[_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],2,M).

square([[_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],3,M).

square([[_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],4,M).

square([[_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],5,M).

square([[_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],6,M).

square([[_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],7,M).

square([[_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],8,M).
        
square([[_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],9,M).

square([[_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],10,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],11,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],12,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],13,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],14,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],15,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],16,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],17,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],18,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],19,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],20,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],21,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],22,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],23,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],24,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],25,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],26,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],27,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],28,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],29,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_], 
        [_,_,_,_,_,_,_]],30,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_], 
        [_,_,_,_,_,_,_]],31,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_], 
        [_,_,_,_,_,_,_]],32,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_], 
        [_,_,_,_,_,_,_]],33,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_], 
        [_,_,_,_,_,_,_]],34,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M], 
        [_,_,_,_,_,_,_]],35,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [M,_,_,_,_,_,_]],36,M).
        
square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,M,_,_,_,_,_]],37,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,M,_,_,_,_]],38,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,M,_,_,_]],39,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,M,_,_]],40,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,M,_]],41,M).

square([[_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,_], 
        [_,_,_,_,_,_,M]],42,M).



%.......................................
% win
%.......................................
% Players win by having their mark in one of the following square configurations:
%

empty(e).

win(Player, Board) :-
    horizontal_win(Player, Board);
    vertical_win(Player, Board);
    diagonal_win(Player, Board).

horizontal_win(Player, Board) :-
    member(Row, Board),
    sublist([Player, Player, Player, Player], Row).

vertical_win(Player, Board) :-
    transpose(Board, Transposed),
    horizontal_win(Player, Transposed).

diagonal_win(Player, Board) :-
    diagonal_up(Board, Player);
    diagonal_down(Board, Player).

diagonal_up(Board, Player) :-
    append(_, [Diagonal|_], Board),
    sublist([Player, Player, Player, Player], Diagonal).

diagonal_down(Board, Player) :-
    reverse(Board, Reversed),
    diagonal_up(Reversed, Player).

sublist(SubList, List) :-
    append(_, Temp, List),
    append(SubList, _, Temp).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Matrix, [Fs|Tss]) :-
    lists_firsts_rests(Matrix, Fs, Ms),
    transpose(Rs, Ms, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in square S on board B and return the resulting board B2)
%

column_pos(Col, S) :-
    board(B),
    column_pos(B, Col, 7, S).

column_pos(B, Col, Row, S) :-
    S is (Row - 1) * 7 + Col,          % Calcul de S basé sur la colonne et la rangée
    square(B, S, E),
    blank_mark(E).                      % Vérifie si la position est vide

column_pos(B, Col, Row, S) :-
    Row > 1,
    NewRow is Row - 1,
    column_pos(B, Col, NewRow, S).  
    
move(B,Col,M,B2) :-
    column_pos(Col, S),
    set_item(B, S, M, B2).



%.......................................
% game_over
%.......................................
% determines when the game is over
%
game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(P, B) :-
    opponent_mark(P, M),   %%% game is over if opponent wins
    win(B, M)
    .

game_over2(P, B) :-
    blank_mark(E),
    not(square(B,S,E))     %%% game is over if opponent wins
    .


%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(P, B) :-
    player(P, Type),

    make_move2(Type, P, B, B2),

    retract( board(_) ),
    asserta( board(B2) )
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(S),

    blank_mark(E),
    square(B, S, E),
    player_mark(P, M),
    move(B, S, M, B2), !
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a numbered square.'),
    make_move2(human,P,B,B2)
    .

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(0, B, M, S, U),
    move(B,S,M,B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in square '),
    write(S),
    write('.')
    .


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B,L) :-
    not(win(B,x)),                %%% if either player already won, then there are no available moves
    not(win(B,o)),
    blank_mark(E),
    findall(N, square(B,N,E), L), 
    L \= []
    .


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U) :-
    win(B,'x'),
    U = 1, 
    !
    .

utility(B,U) :-
    win(B,'o'),
    U = (-1), 
    !
    .

utility(B,U) :-
    U = 0
    .


%.......................................
% output_board
%.......................................
% displays the board


output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
    !
    .


output_winner(B) :-
    win(B,x),
    write('X wins.'),
    !
    .

output_winner(B) :-
    win(B,o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .


print_line(Line) :-
        format("~n\t", []),
        maplist(print_col, Line).

print_col(Col) :- format("~w ", [Col]).

/*
output_board(B) :-
    nl,
    nl,
    output_square(B,1),
    write('|'),
    output_square(B,2),
    write('|'),
    output_square(B,3),
    write('|'),
    output_square(B,4),
    write('|'),
    output_square(B,5),
    write('|'),
    output_square(B,6),
    write('|'),
    output_square(B,7),
    nl,
    .
    
    
    
output_board :-
    board(B),
    output_board(B), !
    .

output_square(B,S) :-
    square(B,S,M),
    write(' '), 
    output_square2(S,M),  
    write(' '), !
    .

output_square2(S, E) :- 
    blank_mark(E),
    write(S), !              %%% if square is empty, output the square number
    .

output_square2(S, M) :- 
    write(M), !              %%% if square is marked, output the mark
    .


% Afficher une seule case.
output_square([H|_], 1) :- !, write(H).
output_square([_|T], N) :-
    N > 1,
    N1 is N-1,
    output_square(T, N1).

% Afficher une seule ligne.
output_row([], _).
output_row([H|T], N) :-
    output_square(H, N),
    (T \= [] -> write('|') ; true),
    output_row(T, N).

% Afficher le tableau.
output_board([]).
output_board([H|T]) :-
    length(H, Length),
    output_row(H, Length),
    nl,
    (T \= [] -> write('------') ; true), nl, % ligne de séparation
    output_board(T).


output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D,S,U) :- 
    true
    .

*/

% Ceci est votre code.
output_square(B, S) :-
    square(B, S, M),
    write(' '),
    output_square2(S, M),
    write(' '), !.

output_square2(S, E) :- 
    blank_mark(E),
    write(S), !.

output_square2(_, M) :- 
    write(M), !.

output_square(B, S, M) :-
    write(' '),
    (blank_mark(M) -> write(S) ; write(M)),
    write(' '), !.

% Ceci est le code pour obtenir la valeur d'une case.
square(B, S, M) :-
    Row is (S-1) div 7 + 1,
    Col is (S-1) mod 7 + 1,
    nth1(Row, B, R),
    nth1(Col, R, M).

% Si E est un marqueur pour une case vide.
blank_mark(E) :- var(E).

% Afficher une seule ligne.
output_row(_, [], _).
output_row(B, [H|T], N) :-
    S is (N-1)*7 + 1, % calculate the starting square number for this row
    output_square(B, S, H),
    (T \= [] -> write('|') ; true),
    S1 is S + 1,
    output_row(B, T, N, S1).

output_row(_, [], _, _).
output_row(B, [H|T], N, S) :-
    output_square(B, S, H),
    (T \= [] -> write('|') ; true),
    S1 is S + 1,
    output_row(B, T, N, S1).

% Afficher le tableau.
output_board(B) :-
    output_board(B, 1).

output_board([], _).
output_board([H|T], N) :-
    output_row(B, H, N),
    nl,
    (T \= [] -> write('------') ; true), nl, % ligne de séparation
    N1 is N + 1,
    output_board(T, N1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PSEUDO-RANDOM NUMBERS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N), 
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
    randomize(N), 
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,           
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member([V|T], V).
member([_|T], V) :- member(T,V).

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).


%.......................................
% set_item
%.......................................
% Given a list L, replace the item at position N with V
% return the new list in list L2
%

set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2)
        .

set_item2( [], N, V, A, L2) :- 
    N == -1, 
    L2 = []
    .

set_item2( [_|T1], N, V, A, [V|T2] ) :- 
    A = N,
    A1 is N + 1,
    set_item2( T1, -1, V, A1, T2 )
    .

set_item2( [H|T1], N, V, A, [H|T2] ) :- 
    A1 is A + 1, 
    set_item2( T1, N, V, A1, T2 )
    .


%.......................................
% get_item
%.......................................
% Given a list L, retrieve the item at position N and return it as value V
%

get_item(L, N, V) :-
    get_item2(L, N, 1, V)
    .

get_item2( [], _N, _A, V) :- 
    V = [], !,
    fail
        .

get_item2( [H|_T], N, A, V) :- 
    A = N,
    V = H
    .

get_item2( [_|T], N, A, V) :-
    A1 is A + 1,
    get_item2( T, N, A1, V)
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%