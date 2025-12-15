:- consult(board). 
:- consult(ia_minimax). 

get_cell(Board, ColIndex, RowIndex, Cell) :-
    nth1(ColIndex, Board, Col),
    nth1(RowIndex, Col, Cell).  

% take N elements from a list
take(0, _, []).
take(N, [H|T], [H|R]) :-
    N > 0,
    N1 is N-1,
    take(N1, T, R).

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

isFullColumn(Col) :-
    length(Col, L),
    L >= 6.

isFullBoard([]).
isFullBoard([Col|Rest]) :-
    isFullColumn(Col),
    isFullBoard(Rest).

game_over(Board, Winner) :-
    win(Board, Winner), 
    !.

game_over(Board, 'Draw') :-
    isFullBoard(Board).

output_winner('Draw') :-
    writeln("La partie se termine par une :"),
    writeln("nulle"), !.

output_winner(Winner) :-
    writeln("Le gagnant est :"),
    writeln(Winner).

change_player('x','o').
change_player('o','x').

insert_in_column(Player, Column, NewColumn) :-
    append(Column, [Player], NewColumn).

replace_column(Board, Index, NewColumn, NewBoard) :-
    nth1(Index, Board, _, RestBoard),
    nth1(Index, NewBoard, NewColumn, RestBoard). 

ask_column(Column) :-
    writeln("Choisissez une colonne (entre 1-7) :"),
    read(Column),
    integer(Column),
    Column >= 1,
    Column =< 7, 
    !.

ask_column(Column) :-
    writeln("Colonne invalide ! Recommencez."),
    ask_column(Column).

valid_move(Board, Index) :-
    nth1(Index, Board, Column),
    length(Column, Length),
    Length < 6.



% Choix IA pour x
choose_column(Board, 'x', Col) :-
    ai_minimax_move(Board, 4, Col).

% Choix IA pour o
choose_column(Board, 'o', Col) :-
     ia_naive_move(Board, 'o', Col).


make_move(Board, Player, NewBoard) :-
    choose_column(Board, Player, Column),
    (   valid_move(Board, Column)
    ->  nth1(Column, Board, OldColumn),
        insert_in_column(Player, OldColumn, NewColumn),
        replace_column(Board, Column, NewColumn, NewBoard)
    ;   
        make_move(Board, Player, NewBoard)
    ).


play(Player) :-
    writeln(['Tour de', Player]),
    board(Board),
    display_current_board,
    sleep(1),   
    (   game_over(Board, Winner)
    ->  output_winner(Winner)
    ;   make_move(Board, Player, NewBoard),
        applyIt(NewBoard),
        change_player(Player, NextPlayer),
        play(NextPlayer)
    ).
