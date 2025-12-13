/* --- Positional score --- */
% Weights matrix
weight(1,1,0.1). weight(1,2,0.15). weight(1,3,0.2). weight(1,4,0.3).
weight(1,5,0.2). weight(1,6,0.15). weight(1,7,0.1).

weight(2,1,0.15). weight(2,2,0.25). weight(2,3,0.35). weight(2,4,0.45).
weight(2,5,0.35). weight(2,6,0.25). weight(2,7,0.15).

weight(3,1,0.2). weight(3,2,0.35). weight(3,3,0.45). weight(3,4,0.5).
weight(3,5,0.45). weight(3,6,0.35). weight(3,7,0.2).

weight(4,1,0.2). weight(4,2,0.35). weight(4,3,0.45). weight(4,4,0.5).
weight(4,5,0.45). weight(4,6,0.35). weight(4,7,0.2).

weight(5,1,0.15). weight(5,2,0.25). weight(5,3,0.35). weight(5,4,0.45).
weight(5,5,0.35). weight(5,6,0.25). weight(5,7,0.15).

weight(6,1,0.1). weight(6,2,0.15). weight(6,3,0.2). weight(6,4,0.3).
weight(6,5,0.2). weight(6,6,0.15). weight(6,7,0.1).

positional_score(Board, Player, Score) :-
    findall(W,
        ( between(1,6,Row),
          between(1,7,Col),
          cell(Board, Col, Row, Player),
          weight(Row, Col, W)
        ),
        Ws),
    sum_list(Ws, Raw),
    Score is min(5, Raw). %max score for positional adventages is 5 

/* --- Tactical score --- */

/* Horizontal windows of length 2 to 5 */
window_horizontal(Board, Col, Row, Window) :-
    between(2,5,Len),
    ColMax is 7 - Len + 1,
    Col =< ColMax,
    Max is Len - 1,
    findall(Cell, 
        (between(0, Max, I), C is Col+I, cell(Board, C, Row, Cell)), 
        Window).

/* Vertical windows of length 2 to 5 */
window_vertical(Board, Col, Row, Window) :-
    between(2,5,Len),
    RowMax is 6 - Len + 1,
    Row =< RowMax,
    Max is Len - 1,
    findall(Cell,
        (between(0, Max, I), R is Row+I, cell(Board, Col, R, Cell)),
        Window).

/* Diagonal up windows of length 2 to 5 */
window_diagonal_up(Board, Col, Row, Window) :-
    between(2,5,Len),
    ColMax is 7 - Len + 1, RowMax is 6 - Len + 1,
    Col =< ColMax, Row =< RowMax,
    Max is Len - 1,
    findall(Cell,
        (between(0, Max, I), C is Col+I, R is Row+I, cell(Board, C, R, Cell)),
        Window).

/* Diagonal down windows of length 2 to 5 */
window_diagonal_down(Board, Col, Row, Window) :-
    between(2,5,Len),
    ColMax is 7 - Len + 1,
    RowMin is Len - 1,
    Col =< ColMax, Row >= RowMin,
    Max is Len - 1,
    findall(Cell,
        (between(0, Max, I), C is Col+I, R is Row-I, cell(Board, C, R, Cell)),
        Window).

consecutive_player_score(Player, Window, Score) :-
    length(Window, Len),
    Len >= 2,
    Window = [_,P2|Rest], 
    P2 == Player,
    consecutive_right(Player, [P2|Rest], 0, Count),
    window_block(Player, Window, Count, Score), !.
consecutive_player_score(_, _, 0).

/* Count consecutive Player pieces from start of the list */
consecutive_right(_, [], Count, Count).
consecutive_right(Player, [H|T], Acc, Count) :-
    ( H == Player -> Acc1 is Acc+1 ; Acc1 = Acc ),
    ( H == Player -> consecutive_right(Player, T, Acc1, Count) ; Count = Acc ).

window_block(Player, Window, Count, Score) :-
    Window = [Left|Rest],
    take(Count, Rest, Xseq),
    append(Xseq, Remaining, Rest),
    ( Remaining = [Next|_] -> (Next == 0 -> RightFree = 1 ; RightFree = 0) ; RightFree = 0 ),
    ( Left == 0 -> LeftFree = 1 ; LeftFree = 0 ),
    Free is LeftFree + RightFree,
    evaluate_block_window(Count, Free, Score).

/* Evaluation rules */
evaluate_block_window(4, _, 100).
evaluate_block_window(3, 2, 20).
evaluate_block_window(3, 1, 3).
evaluate_block_window(2, 2, 2).
evaluate_block_window(2, 1, 1).
evaluate_block_window(_, _, 0).

/* Extract windows where Player is in 2nd position */
player_windows(Board, Player, Windows) :-
    findall(Window,
        (
            between(1,6,Row), between(1,7,Col),
            ( window_horizontal(Board, Col, Row, Window) ;
              window_vertical(Board, Col, Row, Window) ;
              window_diagonal_up(Board, Col, Row, Window) ;
              window_diagonal_down(Board, Col, Row, Window)
            ),
            length(Window, Len),
            Len >= 2,
            Window = [_,Player|_]  % Player in 2nd position
        ),
        Windows
    ).

/* Tactical score for Player */
tactical_score(Board, Player, Score) :-
    player_windows(Board, Player, Windows),
    findall(S,
        ( member(W, Windows),
          consecutive_player_score(Player, W, S)
        ),
        Scores),
    sum_list(Scores, Score).


/* Evaluation function */
evaluate(Board, Player, Score) :-
    positional_score(Board, Player, PosScore),
    tactical_score(Board, Player, TacScore),
    Score is PosScore + TacScore.

/* rules: 
4 in a row  : +100
3 in a row (2 free spots) : +20 
3 in a row (1 free spot) : +3
2 in a row (2 free spots) : +2
2 in a row (1 free spot) : +1 
Positional score : from +0 to +5
*/