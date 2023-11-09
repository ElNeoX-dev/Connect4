%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%     FACTS     %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define large negative and positive numbers
positive_infinity(1000000).
negative_infinity(-1000000).

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
    write('Is human playing x or o (x moves first)? '),
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
    (M == 'x'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :-
    (M == 'o'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter x or o.'),
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
    (between(1, 4, Index);between(8,11,Index);between(15,18,Index)),
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
    play_move(Player, S, Board, NewBoard).


make_move(P, B) :-
    player(P, Type), !,
    make_move2(Type, P, B, B2), !,
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

    (S >= 1, S =< 7,valid_move(S,B) ->  % Check if move is valid
        make_move3(M, S, B, B2), !
    ;
        write('Move is invalid. Please enter a number between 1 and 7 in a non-empty column.'), nl,
        make_move2(human, P, B, B2)
    ).
    

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),

    % minimax(B, M, S, U),
    % move(B,S,M,B2), 

    % Set initial values for Alpha and Beta
    negative_infinity(Alpha), % or a suitably large negative number
    positive_infinity(Beta),   % or a suitably large positive number
    Depth = 5,    % for example, to set the depth of search to 4 levels

    minimax_ab(B, M, Depth, Alpha, Beta, BestMove, BestScore),

    (BestMove == null ->
        findall(Move, valid_move(Move, B), PossibleMoves),
        (   PossibleMoves = [] -> 
            write('No moves left to play - game might be over'), nl,
            FinalMove = null
        ;   [FirstMove|_] = PossibleMoves,
            % Assign the first valid move as the final move
            FinalMove = FirstMove
        )
    ;   % If the BestMove is not null, use it as the final move
        FinalMove = BestMove
    ),
    % Now, use FinalMove with make_move3
    % Make sure FinalMove is not null before making a move
    (FinalMove \== null ->

        make_move3(M, FinalMove, B, B2),
        nl,
        nl,
        write('Computer places '),
        write(M),
        write(' in column '),
        write(FinalMove),
        write('.'),
        nl,nl
    ;   % Handle the case where FinalMove is null (no moves left)
        write('No valid moves to execute.'), nl,
        goodbye
    ).




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
    nth1(S, TransposedBoard, Column),
    member('e', Column).

% Play a move and update the game board.
play_move(Player, S, Board, NewBoard) :-
    transpose(Board, TransposedBoard),
    nth1(S, TransposedBoard, Column),
    reverse(Column, ReversedColumn),  % Reverse to work from the bottom
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
% Utility 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_line('x', ['x','x','x','x'], 1000) :- !.
score_line('x', ['x','x','x','e'], 100) :- !.
score_line('x', ['x','x','e','x'], 100) :- !.
score_line('x', ['x','e','x','x'], 100) :- !.
score_line('x', ['e','x','x','x'], 100) :- !.
score_line('x', ['x','x','e','e'], 10) :- !.
score_line('x', ['x','e','x','e'], 10) :- !.
score_line('x', ['x','e','e','x'], 10) :- !.
score_line('x', ['e','x','x','e'], 10) :- !.
score_line('x', ['e','x','e','x'], 10) :- !.
score_line('x', ['e','e','x','x'], 10) :- !.
score_line('x', ['x','e','e','e'], 1) :- !.
score_line('x', ['e','x','e','e'], 1) :- !.
score_line('x', ['e','e','x','e'], 1) :- !.
score_line('x', ['e','e','e','x'], 1) :- !.

score_line('o', ['o','o','o','o'], -1000) :- !.
score_line('o', ['o','o','o','e'], -100) :- !.
score_line('o', ['o','o','e','o'], -100) :- !.
score_line('o', ['o','e','o','o'], -100) :- !.
score_line('o', ['e','o','o','o'], -100) :- !.
score_line('o', ['o','o','e','e'], -10) :- !.
score_line('o', ['o','e','o','e'], -10) :- !.
score_line('o', ['o','e','e','o'], -10) :- !.
score_line('o', ['e','o','o','e'], -10) :- !.
score_line('o', ['e','o','e','o'], -10) :- !.
score_line('o', ['e','e','o','o'], -10) :- !.
score_line('o', ['o','e','e','e'], -1) :- !.
score_line('o', ['e','o','e','e'], -1) :- !.
score_line('o', ['e','e','o','e'], -1) :- !.
score_line('o', ['e','e','e','o'], -1) :- !.

score_line(_, _, 0).

% Utility predicates to get cell content based on row and column
cell2(Board, Row, Col, Cell) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell).

% Predicate to calculate the score for a diagonal going from top-left to bottom-right
diagonal_score_down(Board, Player, Score) :-
    findall(S, (
        between(1, 3, StartRow),  % The start row for the diagonal can be from 1 to 3
        between(1, 4, StartCol),  % The start column for the diagonal can be from 1 to 4
        diagonal_cells(Board, StartRow, StartCol, 1, 1, Cells), % Collect the four cells in the diagonal
        score_line(Player, Cells, S)  % Calculate the score for the line
    ), Scores),
    sum_list(Scores, Score).

% Predicate to calculate the score for a diagonal going from bottom-left to top-right
diagonal_score_up(Board, Player, Score) :-
    findall(S, (
        between(4, 6, StartRow),  % The start row for the diagonal can be from 4 to 6
        between(1, 4, StartCol),  % The start column for the diagonal can be from 1 to 4
        diagonal_cells(Board, StartRow, StartCol, -1, 1, Cells), % Collect the four cells in the diagonal
        score_line(Player, Cells, S)  % Calculate the score for the line
    ), Scores),
    sum_list(Scores, Score).

% Helper predicate to collect cells from a diagonal
diagonal_cells(Board, Row, Col, RowInc, ColInc, [Cell1, Cell2, Cell3, Cell4]) :-
    cell2(Board, Row, Col, Cell1),
    NextRow1 is Row + RowInc, NextCol1 is Col + ColInc,
    cell2(Board, NextRow1, NextCol1, Cell2),
    NextRow2 is NextRow1 + RowInc, NextCol2 is NextCol1 + ColInc,
    cell2(Board, NextRow2, NextCol2, Cell3),
    NextRow3 is NextRow2 + RowInc, NextCol3 is NextCol2 + ColInc,
    cell2(Board, NextRow3, NextCol3, Cell4).

% Score calculation based on existing score_line predicates
% It sums the scores for diagonals in both directions
total_diagonal_score(Board, Player, TotalScore) :-
    diagonal_score_down(Board, Player, ScoreDown),
    diagonal_score_up(Board, Player, ScoreUp),
    TotalScore is ScoreDown + ScoreUp.

% Horizontal score
horizontal_score(Board, Player, Score) :-
    findall(S, (between(1, 6, Row), between(1, 4, Col),
                EndCol is Col + 3,
                consecutive_horizontal_cells(Board, Row, Col, EndCol, Cells),
                score_line(Player, Cells, S)), Scores),
    sum_list(Scores, Score).

% Vertical score
vertical_score(Board, Player, Score) :-
    findall(S, (between(1, 3, Row), between(1, 7, Col),
                EndRow is Row + 3,
                consecutive_vertical_cells(Board, Row, Col, EndRow, Cells),
                score_line(Player, Cells, S)), Scores),
    sum_list(Scores, Score).

% Center score
center_score(Board, Player, Score) :-
    findall(S, (between(2, 6, Row),
                cell(Board, Row, 4, Player), S = 3), CenterScores),
    sum_list(CenterScores, Score).

% Helper predicate to generate a list of cells from a starting point horizontally
consecutive_horizontal_cells(Board, Row, Col, EndCol, Cells) :-
    findall(Cell, (between(Col, EndCol, C), cell(Board, Row, C, Cell)), Cells).

% Helper predicate to generate a list of cells from a starting point vertically
consecutive_vertical_cells(Board, Row, Col, EndRow, Cells) :-
    findall(Cell, (between(Row, EndRow, R), cell(Board, R, Col, Cell)), Cells).

% Helper predicate to generate a list of cells from a starting point diagonally (down-right)
consecutive_diagonal_cells_down_right(Board, Row, Col, Cells) :-
    findall(Cell, (between(0, 3, Delta), R is Row + Delta, C is Col + Delta, cell(Board, R, C, Cell)), Cells).

% Helper predicate to generate a list of cells from a starting point diagonally (up-right)
consecutive_diagonal_cells_up_right(Board, Row, Col, Cells) :-
    findall(Cell, (between(0, 3, Delta), R is Row - Delta, C is Col + Delta, cell(Board, R, C, Cell)), Cells).

cell(Board, Row, Col, Player) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = Player.

% Overall utility score
utility3(Board, Player, Score) :-
    horizontal_score(Board, Player, HorizScore),
    vertical_score(Board, Player, VertScore),
    % diagonal_score(Board, Player, DiagScore),
    total_diagonal_score(Board, Player, DiagScore),
    center_score(Board, Player, CenterScore),
    maplist(number, [HorizScore, VertScore, DiagScore, CenterScore]),  % This ensures all are numbers
    Score is HorizScore + VertScore + DiagScore + CenterScore.

utility4(Board, PlayerX, PlayerO, NewScore) :-
    utility3(Board, PlayerX, ScoreX),  
    utility3(Board, PlayerO, ScoreO), 
    NewScore is ScoreX + ScoreO.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
%.......................................
% minimax
%.......................................

% Minimax algorithm
minimax(Board, Player, BestMove, BestScore) :-
    findall(Move, valid_move(Move, Board), MovesUnfiltered),
    list_to_set(MovesUnfiltered, Moves),  % Removes duplicates while preserving the order.
    
    InitialScore is 0,
    number(InitialScore),

    best_move(Board, Moves, Player, null, InitialScore, BestMove, BestScore).

best_move(_, [], _, BestMove, BestScore, BestMove, BestScore) .

best_move(Board, [Move|Moves], Player, CurrentBestMove, CurrentBestScore, BestMove, BestScore) :-
    % move(Board, Move, Player, NewBoard),
    make_move3(Player, Move, Board, NewBoard),
    minimax_score(NewBoard, Player, Score),
    better_move(Player, Move, Score, CurrentBestMove, CurrentBestScore, NextBestMove, NextBestScore),
    best_move(Board, Moves, Player, NextBestMove, NextBestScore, BestMove, BestScore).

% Check if the given score is better than the current best score
better_score(Player, Score1, Score2) :-
    (maximizing(Player) -> Score1 > Score2 ; minimizing(Player) -> Score1 < Score2).

% Determine if the current move is better than the best one so far
better_move(Player, Move1, Score1, CurrentBestMove, CurrentBestScore, Move1, Score1) :-
    better_score(Player, Score1, CurrentBestScore).

better_move(Player, _, Score1, CurrentBestMove, CurrentBestScore, CurrentBestMove, CurrentBestScore) :-
    \+ better_score(Player, Score1, CurrentBestScore).


%.......................................
% minimax_score
%.......................................


% Calculate score for the given board and player
minimax_score(Board, Player, Score) :-
    utility4(Board, 'x', 'o', Score).


%.......................................
% alpha beta
%.......................................


% Minimax algorithm with alpha-beta pruning and limited depth
minimax_ab(Board, Player, Depth, Alpha, Beta, BestMove, BestScore) :-
    
    (Depth = 0 ->
        minimax_score(Board, Player, BestScore),
        BestMove = null  % No move because we re at leaf node
    ;
        findall(Move, valid_move(Move, Board), MovesUnfiltered),
        list_to_set(MovesUnfiltered, Moves),
        (maximizing(Player) ->
            negative_infinity(InitialScore);
            positive_infinity(InitialScore)
        ),
        best_move_ab(Board, Moves, Player, Depth, Alpha, Beta, null, InitialScore, BestMove, BestScore)
    ).

best_move_ab(_, [], _, _, _, _, BestMove, BestScore, BestMove, BestScore).

best_move_ab(Board, [Move|Moves], Player, Depth, Alpha, Beta, CurrentBestMove, CurrentBestScore, BestMove, BestScore) :-
    make_move3(Player, Move, Board, NewBoard),
    NewDepth is Depth - 1,
    inverse_mark(Player, OtherPlayer),
    minimax_ab(NewBoard, OtherPlayer, NewDepth, Alpha, Beta, _, OpponentBestScore),
    (maximizing(Player) ->
        NewAlpha is max(Alpha, OpponentBestScore),
        (NewAlpha >= Beta ->
            BestMove = Move,
            BestScore = Beta;
            update_best_move(Player, Move, OpponentBestScore, CurrentBestMove, CurrentBestScore, NextBestMove, NextBestScore),
            best_move_ab(Board, Moves, Player, Depth, NewAlpha, Beta, NextBestMove, NextBestScore, BestMove, BestScore));
        NewBeta is min(Beta, OpponentBestScore),
        (Alpha >= NewBeta ->
            BestMove = Move,
            BestScore = Alpha;
            update_best_move(Player, Move, OpponentBestScore, CurrentBestMove, CurrentBestScore, NextBestMove, NextBestScore),
            best_move_ab(Board, Moves, Player, Depth, Alpha, NewBeta, NextBestMove, NextBestScore, BestMove, BestScore))
    )
    .

update_best_move(Player, Move, Score, CurrentBestMove, CurrentBestScore, NextBestMove, NextBestScore) :-
    better_score(Player, Score, CurrentBestScore),
    !,
    NextBestMove = Move,
    NextBestScore = Score.
update_best_move(_, _, _, CurrentBestMove, CurrentBestScore, CurrentBestMove, CurrentBestScore).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
