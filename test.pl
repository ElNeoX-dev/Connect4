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
    initialize,          %%% Display welcome message, initialize game
    write('Welcome to Connect 4!'),
    read_players,
    output_players,
    play(1)        %%% Play the game starting with player 1
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
    %not(game_over(P, B)), !,
    %make_move(P, B), !,
    %next_player(P, P2), !,
    %play(P2), !
    play_game('x', B),
    output_board(B), !
    .



% Make a move in the game, replacing the first empty cell from the bottom.
make_move(Player, Column, Board, NewBoard) :-
    valid_move(Column, Board),
    play_move(Player, Column, Board, NewBoard).

% Check if a move is valid (the column is not full).
% Takes the nth1 column of the board
% And checks if there is an empty cell in it.
valid_move(Column, Board) :-
    nth1(Column, Board, ColumnList), 
    member('e', ColumnList).

% Play a move and update the game board.
play_move(Player, Column, Board, NewBoard) :-
    transpose(Board, TransposedBoard),
    nth1(Column, TransposedBoard, ColumnList),
    reverse(ColumnList, ReversedColumn),  % Reverse to work from the bottom
    replace_empty(Player, ReversedColumn, ReversedNewColumn),
    reverse(ReversedNewColumn, NewColumn),
    replace_in_list(Column, NewColumn, TransposedBoard, UpdatedTransposedBoard),
    transpose(UpdatedTransposedBoard, NewBoard),
    write('Done'), nl.

% Replace an item in a list with a new item based on the index.
replace_in_list(Index, Value, Original, Replaced) :-
    nth1(Index, Original, _, WithoutOldValue),
    nth1(Index, Replaced, Value, WithoutOldValue).

% Replace the first empty cell in a list with a given value.
replace_empty(X, [H|T], [X|T]) :-
    H = 'e'.
replace_empty(X, [H|T], [H|R]) :-
    H \= 'e',
    replace_empty(X, T, R).


% Play the game.
play_game(Player, Board) :-
    write('Player '), write(Player), write('\'s turn.'), nl,
    repeat,  % This starts a repeat loop, which will continue until a valid input is provided.
    write('Enter column (1-7) to make your move: '),
    read(Column),
    (Column >= 1, Column =< 7 ->  % Check if Column is within the valid range.
        make_move(Player, Column, Board, NewBoard),
        write('Move played'), nl, nl,
        output_board(NewBoard),
        (win(Player, NewBoard) -> write('Player '), write(Player), write(' wins!'), nl;
         switch_player(Player, NextPlayer),
         write('Next player: '), write(NextPlayer), nl, nl,
         play_game(NextPlayer, NewBoard))
    ;
        write('Invalid column. Please enter a number between 1 and 7.'), nl,
        fail  % This causes the repeat loop to restart.
    ).


%.......................................
% square
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

square(Board, Index, M) :-
    Row is (Index - 1) div 7 + 1,
    Col is (Index - 1) mod 7 + 1,
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, M).

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
    win(M,B)
    .

game_over2(P, B) :-
    blank_mark(E),
    not(square(B,S,E))     %%% game is over if board is full
    .


%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%
/*
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

*/
%.......................................
% moves
%.......................................
% retrieves a list of available columns
%

% moves(B,L) :-
%    not(win(B,x)),                %%% if either player already won, then there are no available moves
%    not(win(B,o)),
%    blank_mark(E),
%    findall(N, square(B,N,E), L), 
%    L \= []
%    .

moves(B, L) :-
    not(win(B, x)),
    not(win(B, o)),
    findall(Col, can_move_in_column(B, Col), L),
    L \= [].

% Predicate to check if a move can be made in a given column
can_move_in_column(B, Col) :-
    nth1(Col, B, Column),
    member(empty, Column).  % There's still an empty spot in this column



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
    nl,
    nl,
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

/*
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
*/

output_square(B, S, M) :-
    write(' '),
    (blank_mark(M) -> write('.')
    ; M = 'x' -> write_red(M)
    ; M = 'o' -> write_yellow(M)
    ; write(M)
    ),
    write(' '), !.

% Si E est un marqueur pour une case vide.
blank_mark(E) :- var(E).

% Afficher une seule ligne.
output_row(_, [], _).
output_row(B, [H|T], N) :-
    S is (N-1)*7 + 1, % calculate the starting square number for this row
    output_square(B, S, H),
    (T \= [] -> write_blue('|') ; true),
    S1 is S + 1,
    output_row(B, T, N, S1).

output_row(_, [], _, _).
output_row(B, [H|T], N, S) :-
    output_square(B, S, H),
    (T \= [] -> write_blue('|') ; true),
    S1 is S + 1,
    output_row(B, T, N, S1).

% Afficher le tableau.
output_board(B) :-
    write(' 1   2   3   4   5   6   7 '), nl,
    output_board(B, 1),
    write_blue('–––––––––––––––––––––––––––'), nl.

output_board([], _).
output_board([H|T], N) :-
    output_row(B, H, N),
    (T \= [] -> write('') ; true), nl, % ligne de séparation
    N1 is N + 1,
    output_board(T, N1).


write_red(Text) :- 
    format('~c[31m~w~c[0m', [27, Text, 27]).

write_yellow(Text) :-     
    format('~c[32m~w~c[0m', [27, Text, 27]).

write_blue(Text) :- 
    format('~c[34m~w~c[0m', [27, Text, 27]).



% ?- B = [[x,o,x,o,x,o,x], [x,o,x,o,x,o,x], [x,o,x,o,x,o,x], [x,o,x,o,x,o,x], [x,o,x,o,x,o,x], [x,o,x,o,x,o,x], [x,o,x,o,x,o,x]], output_board(B).



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

member(X, [X|_]).
member(X, [_|T]) :- member(X,T).

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
/*
% Initial game state with an empty board.
initial_board([
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).
*/

/*
% Display the game board.
display_board(Board) :-
    nl,
    write('  1 2 3 4 5 6 7'), nl,
    display_rows(Board, 1).

display_rows([], _).

display_rows([Row | RestRows], RowNumber) :-
    write('|'),
    display_row(Row),
    nl,
    NextRowNumber is RowNumber + 1,
    display_rows(RestRows, NextRowNumber).

display_row([]) :- write(' ').

display_row([Cell | RestCells]) :-
    write(Cell),
    write('|'),
    display_row(RestCells).
*/