/* --- Positional score --- */
weights([
    [0.1, 0.15, 0.2, 0.2, 0.15, 0.1],
    [0.15, 0.25, 0.35, 0.35, 0.25, 0.15],
    [0.2, 0.35, 0.45, 0.45, 0.35, 0.2],
    [0.3, 0.45, 0.5, 0.5, 0.45, 0.3],
    [0.2, 0.35, 0.45, 0.45, 0.35, 0.2],
    [0.15, 0.25, 0.35, 0.35, 0.25, 0.15],
    [0.1, 0.15, 0.2, 0.2, 0.15, 0.1]
]).

positional_score(Board, Player, Score) :-
    weights(Weights),
    findall(W,
        ( between(1,6,Row),
          between(1,7,Col),
          get_cell(Board, Col, Row, Player),
          get_cell(Weights, Col, Row, W)
        ),
        Ws),
    sum_list(Ws, Score).

/* --- Tactical score --- */

dir( 1, 0).   
dir( 0, 1).   
dir( 1, 1).   
dir( 1,-1).   

window4(Board, X, Y, DX, DY, [C1,C2,C3,C4]) :-
    cell(Board, X, Y, C1),
    X2 is X+DX, Y2 is Y+DY, cell(Board, X2, Y2, C2),
    X3 is X+2*DX, Y3 is Y+2*DY, cell(Board, X3, Y3, C3),
    X4 is X+3*DX, Y4 is Y+3*DY, cell(Board, X4, Y4, C4).


window_stats(Player, Window, Count, Free) :-
    include(==(Player), Window, Ps),
    length(Ps, Count),
    include(==(empty), Window, Es),
    length(Es, Free),
    Count > 0.

window_score(4, _, 100) :- !.
window_score(3, 2, 20) :- !.
window_score(3, 1, 3)  :- !.
window_score(2, 2, 2)  :- !.
window_score(2, 1, 1)  :- !.
window_score(_, _, 0).

tactical_score(Board, Player, Score) :-
    findall(S,
        (
            between(1,7,X),
            between(1,6,Y),
            cell(Board, X, Y, Player),
            dir(DX,DY),
            window4(Board, X, Y, DX, DY, Window),
            window_stats(Player, Window, Count, Free),
            Count + Free >= 4,
            window_score(Count, Free, S),
            S > 0
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
