%mettre un jeton dans un colonne

drop_token(Board, ColIndex, Player, NewBoard) :-
    nth1(ColIndex, Board, Col),
    length(Col, L),
    L < 6,                                 % colonne non pleine
    append(Col, [Player], NewCol),         % ajout en tête
    replace(Board, ColIndex, NewCol, NewBoard).


%remplacer une colonne dans le board

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I2 is I - 1,
    replace(T, I2, X, R).


% Définition des joueurs
%---------------------------------
max_player('x').   % l’IA
min_player('o').   % l’humain


utility(Board, 100) :- win(Board, 'x'), !.
utility(Board, -100) :- win(Board, 'o'), !.
utility(_, 0).



% Coups valides
%---------------------------------
valid_moves(Board, Moves) :-
    findall(Col,
        (between(1,7,Col),
         nth1(Col, Board, Column),
         length(Column, L),
         L < 6),
        Moves).


% Minimax principal 
%---------------------------------
minimax(Board, Depth, MaxDepth, _, none, Value) :-
    % Si partie terminée ou profondeur max atteinte
    (win(Board, 'x') ; win(Board, 'o') ; Depth >= MaxDepth),
    utility(Board, Value),
    !.

minimax(Board, Depth, MaxDepth, Player, BestCol, Value) :-
    Depth < MaxDepth,
    valid_moves(Board, Moves),
    Moves \= [],
    D2 is Depth + 1,
    evaluate_moves(Board, D2, MaxDepth, Player, Moves, BestCol, Value).



% Évaluation des coups possibles 
%---------------------------------
evaluate_moves(Board, Depth, MaxDepth, Player, [Col], Col, Value) :-
    !,  
    drop_token(Board, Col, Player, NewBoard),
    change_player(Player, Next),
    minimax(NewBoard, Depth, MaxDepth, Next, _, Value).

evaluate_moves(Board, Depth, MaxDepth, Player, [Col|Rest], BestCol, BestValue) :-
    drop_token(Board, Col, Player, NewBoard),
    change_player(Player, Next),
    minimax(NewBoard, Depth, MaxDepth, Next, _, Value1),
    evaluate_moves(Board, Depth, MaxDepth, Player, Rest, Col2, Value2),
    better(Player, Col, Value1, Col2, Value2, BestCol, BestValue).


% Comparaison de coups
%---------------------------------
better('x', Col1, V1, _, V2, Col1, V1) :- V1 > V2, !.
better('o', Col1, V1, _, V2, Col1, V1) :- V1 < V2, !.
better(_, _, _, Col2, V2, Col2, V2).


% Coup IA 
%---------------------------------
ai_minimax_move(Board, Col) :-
            MaxDepth = 2,
            minimax(Board, 0, MaxDepth, 'x', Col, _),
            !.
% col vaut "none" si il y a un gagnant