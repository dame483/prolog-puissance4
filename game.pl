isEmpty(Case) :- nonvar(Case).
isFullBoard([H|T]) :- isEmpty(H),isFullBoard(T).



game_over(Winner) :- board(Board),winner(Board,Winner),!.
game_over('Draw') :- board(Board),isFullBoard(Board).
make_move(Move,Player,Board,NewBoard) :-Board=NewBoard,nth0() .
outpout_winner(Winner):- writeln("Le gagant est :"),writeln(Winner) .
change_player("red","blue").
change_player("blue","red").
play(Player) :- write("Turn of :"),writeln(Player),
board(Board), displayBoard,ia(Board,Move,Player),
make_move(Move,Player,Board,NewBoard),change_player(Player,NextPlayer)
play(NextPlayer).

