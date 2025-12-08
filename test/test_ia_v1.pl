% Plateau où un coup gagnant existe pour 'x'
test_board_win([
    ['x','x'],  % colonne 1 : l'IA peut gagner ici
    ['o','o'],          
    ['x','x','x'],             
    ['o','o'], 
    [], 
    [], 
    []
]).

% Plateau où aucun coup gagnant n'existe pour 'x'
test_board_no_win([
    ['x','x','x'],  
    ['o','x'],      
    ['x','o'],      
    ['x'],          
    ['o','o','o'],  
    [],             
    []              
]).

% ------------------------
% Test winning_move/3
% ------------------------
test_winning_move :-
    % Plateau où un coup gagnant existe pour x
    test_board_win(B1),

    (winning_move(B1, 'x', Colx) ->
        format("Plateau 1 : coup gagnant reconnu pour x en colonne ~w~n", [Colx]);
        writeln("Plateau 1 : aucun coup gagnant reconnu pour x !")
    ),

    (winning_move(B1, 'o', Colo) ->
        format("Plateau 1 : coup gagnant reconnu pour o en colonne ~w~n", [Colo]);
        writeln("Plateau 1 : aucun coup gagnant reconnu pour o !")
    ),

    % Plateau où un coup gagnant existe pour x
    test_board_no_win(B2),

    (winning_move(B2, 'x', Colx2) ->
        format("Plateau 2 : coup gagnant reconnu pour x en colonne ~w~n", [Colx2]);
        writeln("Plateau 2 : aucun coup gagnant reconnu pour x !")
    ),

    (winning_move(B2, 'o', Colo2) ->
        format("Plateau 2 : coup gagnant reconnu pour o en colonne ~w~n", [Colo2]);
        writeln("Plateau 2 : aucun coup gagnant reconnu pour o !")
    ).

test_random_move :-
    % exemple de plateau
    Board = [
        ['x','o'], 
        ['x','o','x','x','o','x'], 
        [],       
        ['o'],       
        [], [], []    
    ],
    random_valid_move(Board, Col),
    format("Colonne choisie par random_valid_move : ~w~n", [Col]),
    nth1(Col, Board, Column),
    length(Column, L),
    L < 6,          % vérifie que la colonne n'est pas pleine
    writeln("TEST OK : colonne valide sélectionnée").

