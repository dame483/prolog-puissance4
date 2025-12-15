:- use_module(library(plunit)).     % au chargement du fichier charge la bibliothèque de tests unitaires plunit

:- begin_tests(evaluation_tests).

% Test 1 
test(first_evaluation) :-
    Board = [
        ['x','x','x'],
        ['o','o','o'],
        [], [], [], [], []
    ],
    utility(Board, Value),
    assertion((Value < -0.295, Value > -0.305)). % Léger avantage positionnel pour 'o' car pions + centrés

% Test 2
test(second_evaluation) :-
    Board = [
    ['x','o','x','x'],       
    ['o','x','o'],       
    ['x','o','x','o'],    
    ['o','x','x','o'],        
    ['x','o'],            
    ['o','x','o'],        
    ['x','o'] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value > -5.40, Value < -5.3)).