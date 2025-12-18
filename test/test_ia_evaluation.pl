:- use_module(library(plunit)).     % au chargement du fichier charge la bibliothèque de tests unitaires plunit
:- consult(game).
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
    assertion((Value > -4.40, Value < -4.3)).

% Test 3
test(third_evaluation) :-
    Board = [
    ['o'],       
    ['o'],       
    ['x','x'],    
    [],        
    [],            
    [],        
    [] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value < 1.35, Value > 1.25)).

% Test 4
test(fourth_evaluation) :-
    Board = [
    ['o'],       
    [],       
    ['x'],    
    ['x'],        
    [],            
    ['o'],        
    [] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value < 2.3, Value > 2.2)).

% Test 5
test(fifth_evaluation) :-
    Board = [
    ['o','o'],       
    [],       
    ['x'],    
    ['x'],        
    ['x'],            
    [],        
    ['o'] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value < 19.4, Value > 19.3)).

% Test 6
test(sixth_evaluation) :-
    Board = [
    ['o','o'],       
    [],       
    [],    
    ['x','x','x'],        
    [],            
    [],        
    ['o'] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value < 2.95, Value > 2.85)).

% Test 6
test(seventh_evaluation) :-
    Board = [
    ['o','o'],       
    [],       
    [],    
    ['x','x','x','x'],        
    [],            
    [],        
    ['o','o'] 
    ],
    utility(Board, Value),
    write(Value),
    assertion((Value < 99.3, Value > 99.2)).