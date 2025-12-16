:- use_module(library(plunit)).     % au chargement du fichier charge la bibliothèque de tests unitaires plunit

:- begin_tests(minimax_tests).

% Test 1 : si IA a un coup gagnant et adversaire menace de gagner -> IA choisit coup gagnant
test(win_over_block) :-
    Board = [
        ['x','x','x'],
        ['o','o','o'],
        [], [], [], [], []
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 1).


% Test 2 : IA peut gagner immédiatement -> choisit le coup gagnant
test(ai_wins_verticalement) :-
    Board = [
        ['o','o'], 
        ['x','x','x'], [], [], [], [], []
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 2).

% Test 3 : IA a un coup gagnant horizontalement
test(ai_wins_horizontalement) :-
    Board = [
        ['o','o','o'], 
        ['x','x'], 
        ['x','o'], 
        ['x'], 
        [], [], []
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 5).

% Test 4 : IA a un coup gagnant en diagonal
test(ai_wins_diagonal) :-
    Board = [
        ['o','o','o'], 
        ['o','x','x'], 
        ['o','x'], 
        ['x'], 
        [], [], []
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 1).

% Test 5 : IA doit bloquer un coup gagnant de l’adversaire
test(block_opponent_win) :-
    Board = [
        ['o','o','o'], 
        ['x','x'], 
        ['x','o'], 
        [], [], [], []
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 1).


% Test 6 : En profondeur 2 : IA choisit colonne 6 car colonne 5 et 7 prévoit victoire de adversaire
test(depth2_block_opponent) :-
    Board = [
    ['x','x','x','o','x','x'],      
    ['o','o','o','x','o','o'],      
    ['o','x','o','x','o','x'],     
    ['x','x','o','x','o','o'],   
    ['o','o','x','o','x'],      
    ['x','x'],      
    ['o','o','x','o','x']     
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 6).

% Test 7 : 
test(depth2) :-
    Board = [
    ['x','o','x'],      
    ['o','o','o','x','o','x'],      
    ['o','x','o','x'],     
    ['x','x'],   
    ['o','o','x','o','x','o'],      
    ['x','x'],      
    ['o','o','x','o','x','x']     
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 6).

% Test 8 : 
test(depth2_bis) :-
   Board = [
        ['x','o','x','o','o','o'],           
        ['o','o','x'],               
        ['x','x'],               
        ['x','x','o'],          
        ['x','x','o','o','o','x'],                   
        ['o','o','x','x','o','x'],                   
        ['x','o','x','o','o','x']               
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 3).

% Test 9 : 
test(depth2_bis) :-
   Board = [
    ['o','o'],      
    ['x','o'],      
    ['o','x','o','o'],     
    ['o','o','o'],   
    ['o','x','x'],      
    ['x','o','x','x','x','o'],      
    ['x','x','x','o','x','x']     
    ],
    ai_minimax_move(Board, 2, Col),
    assertion(Col == 4).


% Test 10 : 
test(depth4_bis) :-
   Board = [
    ['o','o'],      
    ['x','o'],      
    ['o','x','o','o'],     
    ['o','o','o'],   
    ['o','x','x'],      
    ['x','o','x','x','x','o'],      
    ['x','x','x','o','x','x']     
    ],
    ai_minimax_move(Board, 4, Col),
    assertion(Col == 4).




:- end_tests(minimax_tests).






