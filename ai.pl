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


% trouve un winning move

winning_move(Board, Player, Col) :-
    between(1, 7, Col),
    drop_token(Board, Col, Player, NewBoard),
    win(NewBoard, Player).


%move valide random

random_valid_move(Board, Col) :- 
    findall(C,
        (between(1,7,C),
         nth1(C, Board, Column),
         length(Column, L),
         L < 6),
        Moves),
    % tirer au hasard
    random_member(Col, Moves).

