:- use_module(library(plunit)).
:- consult(game).  

test_board_win([
    ['x','x'],  
    ['o','o'],          
    ['x','x','x'],             
    ['o','o'], 
    [], 
    [], 
    []
]).

test_board_no_win([
    ['x','x','x'],  
    ['o','x'],      
    ['x','o'],      
    ['x'],          
    ['o','o','o'],  
    [],             
    []              
]).


% Tests pour winning_move/3
%------------------------------------------------
:- begin_tests(winning_move_tests).

test(winning_move_x_win, true(Colx == 3)) :-
    test_board_win(B1),
    winning_move(B1, 'x', Colx),
    ! .

test(winning_move_o_no_win) :-
    test_board_no_win(B2),
    winning_move(B2, 'o', _),
    ! .

test(winning_move_x_no_win) :-
    test_board_no_win(B2),
    winning_move(B2, 'x', _),
    ! .

:- end_tests(winning_move_tests).


% Tests pour random_valid_move/2
%------------------------------------------------
:- begin_tests(random_move_tests).

test(random_move_valid, [true(L < 6)]) :-
    Board = [
        ['x','o'], 
        ['x','o','x','x','o','x'], 
        [],       
        ['o'],       
        [], [], []    
    ],
    random_valid_move(Board, Col),
    nth1(Col, Board, Column),
    length(Column, L).

:- end_tests(random_move_tests).
