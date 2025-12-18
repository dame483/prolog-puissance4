:- use_module(library(plunit)).     % au chargement du fichier charge la bibliothèque de tests unitaires plunit
:- consult(game).
:- begin_tests(game_tests).


test_board([
    ['x','o','x'],
    ['x','o'],
    ['x'],
    ['x'],
    ['o','o','o'],
    [],
    []
]).

test_board2([
    ['x','o','x'],
    ['o','o'],
    ['x','x'],
    ['x'],
    ['o','o','o','o'],
    [],
    []
]).

test_board3([
    ['x','o','x','x'],
    ['o','o','x','x'],
    ['x','x','o'],
    ['x','o'],
    ['o','x','o','o'],
    [],
    []
]).

test_board4([
    ['x','o','x','o'],
    ['o','o','x','x'],
    ['x','x','o'],
    ['x','o','x','o'],
    ['o','x','o','o','o'],
    [],
    []
]).


test_board5([
    ['x','o','x','o','x','o'],
    ['o','x','o','x','o','x'],
    ['x','o','x','o','x','o'],
    ['x','o','x','o','x','o'],
    ['o','x','o','x','o','x'],
    ['x','o','x','o','x','o'],
    ['x','o','x','o','x','o']
]).



test(diagonal_win_down_o) :-
    test_board4(B),
    diagonal_win_down(B, 'o'),
    ! .

test(diagonal_win_up_x) :-
    test_board3(B),
    diagonal_win_up(B, 'x'),
    ! .


test(vertical_win_o) :-
    test_board2(B),
    vertical_win(B, 'o'),
    !.

test(horizontal_win_x, [fail]) :-
    test_board2(B),
    horizontal_win(B, 'x'),
    !.

test(game_over_board3) :-
    test_board3(B),
    game_over(B, _Winner).

test(game_over_board4) :-
    test_board4(B),
    game_over(B, _Winner).

test(game_over_board5) :-
    test_board5(B),
    game_over(B, _Winner).

:- end_tests(game_tests).
