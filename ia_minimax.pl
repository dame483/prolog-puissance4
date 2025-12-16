
:- consult(ia_naive). 
:- consult(evaluation). 

/*% Définition des joueurs
%---------------------------------
max_player('x').   % l’IA
min_player('o').   % l’humain */

/*% Ancienne fonction utility

utility(Board, 100) :- win(Board, 'x'), !.
utility(Board, -100) :- win(Board, 'o'), !.
utility(_, 0).
*/


utility(Board, Value) :-
    evaluate(Board, 'x', ScoreX),
    evaluate(Board, 'o', ScoreO),
    Value is ScoreX - ScoreO.


% Coups valides

valid_moves(Board, Moves) :-
    findall(Col,
        (between(1,7,Col),
         nth1(Col, Board, Column),
         length(Column, L),
         L < 6),
        Moves).


% Minimax principal 

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

better('x', Col1, V1, _, V2, Col1, V1) :- V1 > V2, !.
better('o', Col1, V1, _, V2, Col1, V1) :- V1 < V2, !.
better(_, _, _, Col2, V2, Col2, V2).


% Coup IA 

ai_minimax_move(Board, MaxDepth, Col) :-
    minimax(Board, 0, MaxDepth, 'x', Col, _),
    !.

