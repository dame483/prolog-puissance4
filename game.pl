get_cell(Board, ColIndex, RowIndex, Cell) :-
    nth1(ColIndex, Board, Col),
    nth1(RowIndex, Col, Cell).  

vertical_win(Board, Player) :-
    member(Col, Board),
    append(_, [Player, Player, Player, Player|_], Col).

horizontal_win(Board, Player) :-
    between(1, 7, Col), % Col = {1,2,3,4,5,6,7}
    between(1, 6, Row), % Row = {1,2,3,4,5,6}
    Col2 is Col+1,
    Col3 is Col+2,
    Col4 is Col+3,
    get_cell(Board, Col,  Row, Player),
    get_cell(Board, Col2, Row, Player),
    get_cell(Board, Col3, Row, Player),
    get_cell(Board, Col4, Row, Player).

diagonal_win_down(Board, Player) :-
    between(1, 7, Col),
    between(1, 6, Row),
    Col2 is Col+1,
    Col3 is Col+2,
    Col4 is Col+3,
    Row2 is Row+1,
    Row3 is Row+2,
    Row4 is Row+3,
    get_cell(Board, Col,  Row,  Player),
    get_cell(Board, Col2, Row2, Player),
    get_cell(Board, Col3, Row3, Player),
    get_cell(Board, Col4, Row4, Player).

diagonal_win_up(Board, Player) :-
    between(1, 7, Col),
    between(1, 6, Row),
    Col2 is Col+1,
    Col3 is Col+2,
    Col4 is Col+3,
    Row2 is Row-1,
    Row3 is Row-2,
    Row4 is Row-3,
    Row > 0, Row2 > 0, Row3 > 0, Row4 > 0,
    get_cell(Board, Col,  Row,  Player),
    get_cell(Board, Col2, Row2, Player),
    get_cell(Board, Col3, Row3, Player),
    get_cell(Board, Col4, Row4, Player).

win(Board, Player) :-
    vertical_win(Board, Player);
    horizontal_win(Board, Player);
    diagonal_win_down(Board, Player);
    diagonal_win_up(Board, Player).