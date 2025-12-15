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

/* positional_score/3 : Compute positional score for a player */
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

dir(1, 0).   
dir(0, 1).   
dir(1, 1).   
dir(1,-1).   


/* window/9 : Get a window in a direction (DX,DY) with a Player in the head */
window(Board, X, Y, DX, DY, Player, Length, FreeLeft, FreeRight) :-
    cell(Board, X, Y, Player),
    /* Ensure the Player is in the head of the window */
    XPrev is X - DX, 
    YPrev is Y - DY,
    cell_safe(Board, XPrev, YPrev, Before),
    Before \= Player,  
    
    count_consecutive(Board, X, Y, DX, DY, Player, Length),
    Length > 0,
    
    /* Count free spaces before and after the window */
    count_free_spaces(Board, XPrev, YPrev, -DX, -DY, Player, FreeLeft),
    XEnd is X + Length*DX, 
    YEnd is Y + Length*DY,
    count_free_spaces(Board, XEnd, YEnd, DX, DY, Player, FreeRight),

    /* Ensure the window is big enough to make a connect-4*/
    TotalLength is Length + FreeLeft + FreeRight,
    TotalLength >= 4.

/* count_consecutive/7 : Get the number of consecutive Player's pieces in a direction */
count_consecutive(Board, X, Y, DX, DY, Player, Count) :-
    cell_safe(Board, X, Y, Player),
    !,
    XNext is X + DX, 
    YNext is Y + DY,
    count_consecutive(Board, XNext, YNext, DX, DY, Player, RestCount),
    Count is RestCount + 1.
count_consecutive(_, _, _, _, _, _, 0).

/* count_free_spaces/7 : Count the number of free spaces in a direction */
count_free_spaces(Board, X, Y, DX, DY, Player, Count) :-
    count_free_spaces_limited(Board, X, Y, DX, DY, Player, 0, Count).

/* count_free_spaces_limited/8 : Used in count_free_spaces 
Move along a direction and count how many empty spaces there are before encountering:
- the edge of the board (out)
- a player's piece
- or an opponent's piece
*/
count_free_spaces_limited(Board, X, Y, DX, DY, Player, Acc, Count) :-
    cell_safe(Board, X, Y, Cell),
    (Cell = empty ->
        Acc1 is Acc + 1,
        XNext is X + DX,
        YNext is Y + DY,
        count_free_spaces_limited(Board, XNext, YNext, DX, DY, Player, Acc1, Count)
    ; Cell = out ->
        Count = Acc
    ; Cell = Player ->
        Count = Acc
    ;
        Count = Acc
    ).

/* window_score/4 : Compute the score for a window (ensure that a window will be big enough to score points)*/
window_score(Length, FreeLeft, FreeRight, Score) :-
    NeededSpaces is 4 - Length,
    
    UsableFreeLeft is min(FreeLeft, NeededSpaces),
    UsableFreeRight is min(FreeRight, NeededSpaces),
    TotalFree is UsableFreeLeft + UsableFreeRight,
    
    (UsableFreeLeft > 0, UsableFreeRight > 0 -> 
        TwoSides = true 
    ; 
        TwoSides = false
    ),
    
    calculate_score(Length, TotalFree, TwoSides, Score).

/* Tactical score rules */
calculate_score(4, _, _, 100) :- !.
calculate_score(3, TotalFree, true, 20) :- TotalFree >= 1, !.  
calculate_score(3, TotalFree, false, 3) :- TotalFree >= 1, !.  
calculate_score(2, TotalFree, true, 2) :- TotalFree >= 2, !.   
calculate_score(2, TotalFree, false, 1) :- TotalFree >= 2, !.  
calculate_score(_, _, _, 0).

/* tactical_score/3 : Compute tactical score for a player */
tactical_score(Board, Player, Score) :-
    findall(S,
        (
            between(1,7,X),
            between(1,6,Y),
            dir(DX,DY),
            window(Board, X, Y, DX, DY, Player, Length, FreeLeft, FreeRight),
            window_score(Length, FreeLeft, FreeRight, S),
            S > 0
        ),
        Scores),
    sum_list(Scores, Score).


/* evaluate/3 - Board evaluation function */
evaluate(Board, Player, Score) :-
    positional_score(Board, Player, PosScore),
    tactical_score(Board, Player, TacScore),
    Score is PosScore + TacScore.