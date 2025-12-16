:- use_module(library(plunit)).

:- begin_tests(winning_move_tests).


% ------------------------
% Tests winning_move/3
% ------------------------

test(winning_move_x_exists) :-
    B=[
    ['x','x'],
    ['o','o'],
    ['x','x','x'],
    ['o','o'],
    [], [], []
    ],
    winning_move(B, 'x', Col),
    assertion(Col == 3).

test(winning_move_o_not_exists) :-
     B=[
    ['x','x'],
    ['o','o'],
    ['x','x','x'],
    ['o','o'],
    [], [], []
    ],
    \+ winning_move(B, 'o', _).


% ------------------------
% Test random_valid_move/2
% ------------------------

test(random_move_valid) :-
    Board = [
        ['x','o'],
        ['x','o','x','x','o','x'],
        [],
        ['o'],
        [], [], []
    ],
    random_valid_move(Board, Col),
    nth1(Col, Board, Column),
    length(Column, L),
    assertion(L < 6).

:- end_tests(winning_move_tests).
