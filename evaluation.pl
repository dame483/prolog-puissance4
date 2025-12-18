% Definition de la matrice de poids : score positionnel
weights([
    [0.1, 0.15, 0.2, 0.2, 0.15, 0.1],
    [0.15, 0.25, 0.35, 0.35, 0.25, 0.15],
    [0.2, 0.35, 0.45, 0.45, 0.35, 0.2],
    [0.3, 0.45, 0.5, 0.5, 0.45, 0.3],
    [0.2, 0.35, 0.45, 0.45, 0.35, 0.2],
    [0.15, 0.25, 0.35, 0.35, 0.25, 0.15],
    [0.1, 0.15, 0.2, 0.2, 0.15, 0.1]
]).

% Calcule le score d’un joueur selon ses positions sur le plateau
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


% Directions dans lesquelles on regarde pour aligner des pions
dir(1, 0).   
dir(0, 1).   
dir(1, 1).   
dir(1,-1).   


% Récupére une fenêtre dans une direction donnée où le joueur a au moins un pion au début
window(Board, X, Y, DX, DY, Player, Length, FreeLeft, FreeRight) :-
    cell(Board, X, Y, Player),
    /* On s'assure que le pion du joueur est au début de la fenêtre */
    XPrev is X - DX, 
    YPrev is Y - DY,
    cell_safe(Board, XPrev, YPrev, Before),
    Before \= Player,  
    
    count_consecutive(Board, X, Y, DX, DY, Player, Length),
    Length > 0,
    
    /* Compter les espaces libres */
    count_free_spaces(Board, XPrev, YPrev, -DX, -DY, Player, FreeLeft),
    XEnd is X + Length*DX, 
    YEnd is Y + Length*DY,
    count_free_spaces(Board, XEnd, YEnd, DX, DY, Player, FreeRight),

    /* Vérifier que la fenêtre peut devenir un alignement de 4 pions */
    TotalLength is Length + FreeLeft + FreeRight,
    TotalLength >= 4.

% Obtenir le nombre de pièces consécutives du joueur dans une direction
count_consecutive(Board, X, Y, DX, DY, Player, Count) :-
    cell_safe(Board, X, Y, Player),
    !,
    XNext is X + DX, 
    YNext is Y + DY,
    count_consecutive(Board, XNext, YNext, DX, DY, Player, RestCount),
    Count is RestCount + 1.
count_consecutive(_, _, _, _, _, _, 0).



% Compter le nombre de cases libres dans une direction 
count_free_spaces(Board, X, Y, DX, DY, Player, Count) :-
    count_free_spaces_limited(Board, X, Y, DX, DY, Player, 0, Count).

/* Utilisé dans count_free_spaces 
Se déplacer dans une direction et compter combien de cases vides se trouvent avant de rencontrer :
- le bord du plateau (out)
- une pièce du joueur
- ou une pièce de l’adversaire
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

% Calculer le score pour une fenêtre (vérifier fenêtre suffisamment grande pour marquer des points)
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

% Règles de score tactique
calculate_score(4, _, _, 100) :- !.
calculate_score(3, TotalFree, true, 20) :- TotalFree >= 1, !.  
calculate_score(3, TotalFree, false, 3) :- TotalFree >= 1, !.  
calculate_score(2, TotalFree, true, 2) :- TotalFree >= 2, !.   
calculate_score(2, TotalFree, false, 1) :- TotalFree >= 2, !.  
calculate_score(_, _, _, 0).

% Calculer le score tactique pour un joueur
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


% Fonction d’évaluation du plateau 
evaluate(Board, Player, Score) :-
    positional_score(Board, Player, PosScore),
    tactical_score(Board, Player, TacScore),
    Score is PosScore + TacScore.