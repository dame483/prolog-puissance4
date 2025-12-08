init_board(Board) :-
    Board = [[], [], [], [], [], [], []].
display_board(Board) :-
    display_rows(Board, 6).  % on affiche par ligne et on a 6 lignes

display_rows(_, 0) :- !.

display_rows(Board, Row) :- % Parcours des lignes de 6 à 1
    display_row(Board, Row),
    NewRow is Row - 1,
    display_rows(Board, NewRow).

display_row([], _) :- nl.

display_row([Col | Rest], Row) :- % Afficher une ligne entière en récupérant la hauteur maximale
    print_cell(Col, Row),
    write(' '),
    display_row(Rest, Row).

print_cell(Col, Row) :- % Afficher une cellule
    length(Col, Height),
    ( Row =< Height -> nth1(Row, Col, Token);
      Token = '_'
    ),
    write(Token).

applyIt(Board, newBoard) :-
    retract(board(Board)),
    assert(board(newBoard)).

