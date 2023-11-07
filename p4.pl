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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%   MAIN PROGRAM   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run :-
    hello,          %%% Display welcome message, initialize game

    not(play(1)),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

hello :-
    initialize,
%   cls,
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
    asserta(board([[E,E,E,E,E,E,E],
                    [E,E,E,E,E,E,E],
                    [E,E,E,E,E,E,E],
                    [E,E,E,E,E,E,E],
                    [E,E,E,E,E,E,E],
                    [E,E,E,E,E,E,E]]))  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V),
    process_play_again_response(V).

process_play_again_response('y') :-
    run.

process_play_again_response('n') :-
    true.

read_play_again(V) :-
    nl,
    nl,
    write('Play again (y/n)? '),
    read(V),
    (V == 'y' ; V == 'n'), !
    .


read_play_again(V) :-
    nl,
    nl,
    write('Please enter y or n.'),
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

diagonal_down(Board, Player) :-
    between(1, 18, Index),
    square(Board, Index, Player),
    square(Board,Index+8,Player),
    square(Board,Index+16,Player),
    square(Board,Index+24,Player).

diagonal_up(Board, Player) :-
    reverse(Board, Reversed),
    diagonal_down(Reversed, Player).

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
    \+ (between(1, 42, S), square(B, S, E)). %%% game is over if board is full

%.......................................
% make_move
%.......................................
% requests next move from human/computer,
% then applies that move to the given board
%

% Make a move in the game, replacing the first empty cell from the bottom.
make_move3(Player, S, Board, NewBoard) :-
    valid_move(S, Board),
    play_move(Player, S, Board, NewBoard).


make_move(P, B) :-
    player(P, Type),

    make_move2(Type, P, B, B2),

    retract(board(_)),
    asserta(board(B2))
    .


make_move2(human, P, B, B2) :-
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(S),

    blank_mark(E),
    square(B, S, E),
    player_mark(P, M),

    (S >= 1, S =< 7 ->  % Check if Column is within the valid range.
        make_move3(M, S, B, B2), !
    ;
        write('Invalid column. Please enter a number between 1 and 7.'), nl,
        fail
    ).
    

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(B, M, S, U),
    % move(B,S,M,B2), 
    make_move3(M, S, B, B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in square '),
    write(S),
    write('.'),
    nl.


%.......................................
% moves
%.......................................
% retrieves a list of available columns
%

% Check if a move is valid (the column is not full).
% Takes the nth1 column of the board
% And checks if there is an empty cell in it.
valid_move(S, Board) :-
    transpose(Board,TransposedBoard),
    nth1(S, TransposedBoard, ColumnList),
    member('e', ColumnList).

% Play a move and update the game board.
play_move(Player, S, Board, NewBoard) :-
    transpose(Board, TransposedBoard),
    nth1(S, TransposedBoard, ColumnList),
    reverse(ColumnList, ReversedColumn),  % Reverse to work from the bottom
    replace_empty(Player, ReversedColumn, ReversedNewColumn),
    reverse(ReversedNewColumn, NewColumn),
    replace_in_list(S, NewColumn, TransposedBoard, UpdatedTransposedBoard),
    transpose(UpdatedTransposedBoard, NewBoard)
    % write(NewBoard), nl,
    % write('Done'), nl
    .

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


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U) :-
    win('x',B),
    U = 1,
    !
    .

utility(B,U) :-
    win('o',B),
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
    win('x',B),
    write('Player 1 (x) wins.'),
    !
    .

output_winner(B) :-
    win('o',B),
    write('Player 2 (o) wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .


print_line(Line) :-
        format("~n\t", []),
        maplist(print_col, Line).

print_col(Col) :- format("~w ", [Col]).


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
    write_blue('–––––––––––––––––––––––––––'), nl, nl.

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
%%% UTILITY 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simplified utility evaluation
utility2(Board, U) :-
    (   win('x', Board) -> U = 1.0
    ;   win('o', Board) -> U = -1.0
    ;   near_winning_position(Board, 'x') -> U = 0.5
    ;   near_winning_position(Board, 'o') -> U = -0.5
    ;   threats_difference(Board, U)
    ).

% Compute the difference in potential threats
threats_difference(Board, U) :-
    count_potential_threats(Board, 'x', PThreats),
    count_potential_threats(Board, 'o', OThreats),
    U is (PThreats - OThreats) * 0.01.

% Check for near-winning positions
near_winning_position(Board, Player) :-
    (   near_winning_line(Board, Player)
    ;   near_winning_line(transpose(Board), Player)  % Check vertical by transposing
    ;   near_winning_diagonal(Board, Player)
    ).

% Check for near-winning lines (horizontal/vertical)
near_winning_line(Board, Player) :-
    member(Line, Board),
    three_and_one_empty(Player, Line).

% Check for three of a kind and one empty slot
three_and_one_empty(Player, Line) :-
    append(_, [A, B, C, D | _], Line),
    select(empty, [A, B, C, D], Three),
    all_same(Player, Three).

% Confirm all elements in a list match Player
all_same(Player, [H | T]) :-
    H = Player,
    all_same(Player, T).
all_same(_, []).

% Count potential threats
count_potential_threats(Board, Player, Count) :-
    findall(1, potential_threat(Board, Player), Threats),
    length(Threats, Count).

% Define potential threat without explicitly creating a list
potential_threat(Board, Player) :-
    between(1, 7, Col),
    column(Board, Col, ColumnData),
    empty_slot(ColumnData, Row),
    make_move(Board, [Row, Col], Player, NewBoard),
    near_winning_position(NewBoard, Player).

% Find the first empty slot in a column
empty_slot(ColumnData, Row) :-
    nth1(Row, ColumnData, empty).

% Get the nth column of a board
column(Board, N, Column) :-
    maplist(nth1(N), Board, Column).

% Check for near-winning diagonals
near_winning_diagonal(Board, Player) :-
    (   diagonal(Board, Player, Diagonal),
        three_and_one_empty(Player, Diagonal)
    ;   diagonal(reversed_board(Board), Player, Diagonal),
        three_and_one_empty(Player, Diagonal)
    ).

% Extract diagonal from the board
diagonal(Board, Player, Diagonal) :-
    findall(Cell, (between(1, 4, N), nth1(N, Board, Row), nth1(N, Row, Cell)), Diagonal).

% Reverse the board to check the other diagonal
reversed_board(Board, Reversed) :-
    maplist(reverse, Board, ReversedRows),
    reverse(ReversedRows, Reversed).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   

%.......................................
% minimax
%.......................................

% Minimax algorithm with debugging
minimax(Board, Player, BestMove, BestScore) :-
    write('Minimax called for player: '), write(Player), nl,
    findall(Move, valid_move(Move, Board), Moves),
    write('Possible moves: '), write(Moves), nl,
    (
        maximizing(Player) -> 
        InitialScore = -inf;
        minimizing(Player) -> 
        InitialScore = inf
    ),
    best_move(Board, Moves, Player, null, InitialScore, BestMove, BestScore),
    write('Best move chosen: '), write(BestMove), write(' with score: '), write(BestScore), nl.

best_move(_, [], _, BestMove, BestScore, BestMove, BestScore) :-
    write('No moves left. Best move so far: '), write(BestMove), write(' with score: '), write(BestScore), nl.
best_move(Board, [Move|Moves], Player, CurrentBestMove, CurrentBestScore, BestMove, BestScore) :-
    write('Evaluating move: '), write(Move), nl,
    % move(Board, Move, Player, NewBoard),
    make_move3(Player, Move, Board, NewBoard),
    write('After move: '), write(Move), nl,
    write('Before score: '), write(Player), nl,
    minimax_score(NewBoard, Player, Score),
    write('Score for move '), write(Move), write(': '), write(Score), nl,
    better_move(Player, Move, Score, CurrentBestMove, CurrentBestScore, NextBestMove, NextBestScore),
    best_move(Board, Moves, Player, NextBestMove, NextBestScore, BestMove, BestScore).

% Calculate score for the given board and player
minimax_score(Board, Player, Score) :-
    write('Calculating minimax score for player: '), write(Player), nl,
    utility2(Board, Score),
    write('Calculated score: '), write(Score), nl.

% Check if the given score is better than the current best score
better_score(Player, Score1, Score2) :-
    maximizing(Player) -> Score1 > Score2 ; Score1 < Score2.

% Determine if the current move is better than the best one so far
better_move(Player, Move1, Score1, CurrentBestMove, CurrentBestScore, Move1, Score1) :-
    write('Comparing '), write(Score1), write(' with '), write(CurrentBestScore), nl,
    better_score(Player, Score1, CurrentBestScore),
    write('Move '), write(Move1), write(' with score '), write(Score1), write(' is better than current best.'), nl.
better_move(Player, _, Score1, CurrentBestMove, CurrentBestScore, CurrentBestMove, CurrentBestScore) :-
    \+ better_score(Player, Score1, CurrentBestScore),
    write('Current best move '), write(CurrentBestMove), write(' with score '), write(CurrentBestScore), write(' remains the best.'), nl.




/*
minimax(Board, Player, BestMove, BestScore) :-
    write('minimax called for player '), write(Player), nl,
    findall(Move, valid_move(Move, Board), Moves),
    write('Valid moves found: '), write(Moves), nl,
    best_move(Board, Moves, Player, BestMove, BestScore),
    write('Best move: '), write(BestMove), write(' with score '), write(BestScore), nl.

best_move(Board, [Move|Moves], Player, BestMove, BestScore) :-
    write('Evaluating move: '), write(Move), nl,
    move(Board, Move, Player, NewBoard),     % make_move3(Player, Move, Board, NewBoard),
    write('After move: '), write(Move), nl,
    write('Before Opponent Player: '), write(Player), nl,
    inverse_mark(Player, Opponent),
    
    write('Before minimax_score: '), write(Opponent), nl,
    minimax_score(NewBoard, Opponent, Score),
    write('Score for move '), write(Move), write(': '), write(Score), nl,
    
    best_move(Board, Moves, Player, OtherMove, OtherScore),
    better_move(Player, Move, Score, OtherMove, OtherScore, BestMove, BestScore).

best_move(_, [], _, null, -inf) :-
    write('No more moves left, returning -infinity'), nl.

minimax_score(Board, Player, Score) :-
    game_over(Player, Board), % Assuming game_over also returns a reason
    write('Game over '), nl,
    utility2(Board, Score),
    write('Utility score for board: '), write(Score), nl,
    !.

minimax_score(Board, Player, Score) :-
    inverse_mark(Player, Opponent),
    findall(OppMove, valid_move(OppMove, Board), OppMoves),
    write('Opponent moves found: '), write(OppMoves), nl,
    best_opp_move(Board, OppMoves, Opponent, _, OppScore),
    Score is -OppScore,
    write('Opponent score: '), write(OppScore), write(', Score: '), write(Score), nl.

best_opp_move(Board, [Move|Moves], Opponent, BestMove, BestScore) :-
    write('Evaluating opponent move: '), write(Move), nl,
    move(Board, Move, Opponent, NewBoard),    % make_move3(Opponent, Move, Board, NewBoard),
    minimax_score(NewBoard, Opponent, Score),
    best_opp_move(Board, Moves, Opponent, OtherMove, OtherScore),
    better_move(Opponent, Move, Score, OtherMove, OtherScore, BestMove, BestScore).

best_opp_move(_, [], _, null, inf) :-
    write('No more opponent moves left, returning infinity'), nl.

better_move(Player, Move1, Score1, Move2, Score2, Move1, Score1) :-
    write('Comparing scores: '), write(Score1), write(' vs '), write(Score2), nl,
    better_score(Player, Score1, Score2),
    write(Move1), write(' is a better move than '), write(Move2), nl,
    !.

better_move(_, _, _, Move2, Score2, Move2, Score2) :-
    write('Defaulting to move '), write(Move2), nl.

better_score(Player, Score1, Score2) :-
    write('Checking if player '), write(Player), write(' should maximize or minimize'), nl,
    (maximizing(Player) -> Score1 > Score2 ; Score1 < Score2),
    write('Better score for player '), write(Player), write(' is '), write(Score1), nl.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
