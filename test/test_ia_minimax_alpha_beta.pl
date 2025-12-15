:- use_module(library(plunit)).
:- consult('../board.pl').
:- consult('../game.pl').
:- consult('../evaluation.pl').
:- consult('../ia_minimax_alpha_beta.pl').

:- begin_tests(minimax_alpha_beta_tests).

% Alpha-Beta atteint profondeur 6 avec un plateau mid-game
test(alpha_beta_reaches_depth6) :-
    Board = [
        ['x','o'],      
        ['o','x'],      
        ['x','o','x'],     
        ['o','x'],   
        ['x'],      
        [],      
        []     
    ],
    ai_minimax_alpha_beta_move(Board, 6, Col),
    assertion(member(Col, [1, 2, 3, 4, 5, 6, 7])).

% Comparaison multi-profondeur
test(alpha_beta_multi_depth_comparison) :-
    Board = [
        ['x'],      
        ['o'],      
        ['x'],     
        [],   
        [],      
        [],      
        []     
    ],
    ai_minimax_alpha_beta_move(Board, 2, Col2),
    ai_minimax_alpha_beta_move(Board, 4, Col4),
    ai_minimax_alpha_beta_move(Board, 6, Col6),
    % Tous les coups doivent être valides et dans la plage 1-7
    assertion(member(Col2, [1, 2, 3, 4, 5, 6, 7])),
    assertion(member(Col4, [1, 2, 3, 4, 5, 6, 7])),
    assertion(member(Col6, [1, 2, 3, 4, 5, 6, 7])).

:- end_tests(minimax_alpha_beta_tests).

test_board_non_fini([
    ['o','o'],      
    ['x','o'],      
    ['o','x','o','o'],     
    ['o','o','o','o'],   
    ['o','x','x','x'],      
    ['x','o','x','x','x','o'],      
    ['x','x','x','o','x','x']     
    ]).

% ?- test_board_non_fini(B), game_over(B,Winner).
