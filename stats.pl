:- consult(board). 
:- consult(game). 
:- consult(ia_naive). 
:- consult(ia_minimax). 


%boucle du jeu
play_game(Player, Result) :-
    board(Board),
    (   game_over(Board, Winner)
    ->  Result = Winner
    /*->  output_winner(Winner)*/
    ;   make_move(Board, Player, NewBoard),
        applyIt(NewBoard),
        change_player(Player, NextPlayer),
        play_game(NextPlayer, Result)
    ).

%initialisation du jeu
play_init(FirstPlayer, Result) :-
    init_game,
    play_game(FirstPlayer, Result).

%simulation de N jous 
simulate(0, []) :- !.
simulate(N, [R|Rs]) :-
    N > 0,
    /*random_member(FirstPlayer, ['x','o']),*/
    play_init('o', R),
    /*writeln(["Match num:", N]),*/
    N1 is N - 1,
    simulate(N1, Rs).

%fontion de comptage
count(_, [], 0).
count(X, [X|T], N) :-
    count(X, T, N1),
    N is N1 + 1.
count(X, [_|T], N) :-
    count(X, T, N).

%statistiques de simulation
run(N) :-
    simulate(N, Results),
    count('x', Results, MinimaxWins),
    count('o', Results, NaiveWins),
    count('Draw', Results, Draws),
    format('Minimax (x): ~w~nNaive (o): ~w~nDraw: ~w~n',
           [MinimaxWins, NaiveWins, Draws]).



