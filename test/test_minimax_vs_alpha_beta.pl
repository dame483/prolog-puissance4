:- dynamic minimax_stats/3.
:- dynamic alpha_beta_stats/3.

:- consult('../board.pl').
:- consult('../game.pl').
:- consult('../evaluation.pl').
:- consult('../ia_minimax.pl').
:- consult('../ia_minimax_alpha_beta.pl').

alpha_beta_move(Board, Depth, Player, Col) :-
    minimax_ab(Board, 0, Depth, -100000, 100000, Player, Col, _),
    !.

select_move_minimax(Board, 'x', Col) :-
    ai_minimax_move(Board, 3, Col).

select_move_alpha_beta(Board, 'o', Col) :-
    alpha_beta_move(Board, 3, 'o', Col),
    !.

apply_minimax(Board, NewBoard, Index, Duration) :-
    get_time(T1),
    select_move_minimax(Board, 'x', Col),
    get_time(T2),
    Duration is T2 - T1,
    (   valid_move(Board, Col)
    ->  nth1(Col, Board, Old),
        insert_in_column('x', Old, Updated),
        replace_column(Board, Col, Updated, NewBoard),
        assertz(minimax_stats(Index, Col, Duration))
    ;   apply_minimax(Board, NewBoard, Index, Duration)
    ).

apply_alpha_beta(Board, NewBoard, Index, Duration) :-
    get_time(T1),
    select_move_alpha_beta(Board, 'o', Col),
    get_time(T2),
    Duration is T2 - T1,
    (   valid_move(Board, Col)
    ->  nth1(Col, Board, Old),
        insert_in_column('o', Old, Updated),
        replace_column(Board, Col, Updated, NewBoard),
        assertz(alpha_beta_stats(Index, Col, Duration))
    ;   apply_alpha_beta(Board, NewBoard, Index, Duration)
    ).

show_state(Board, Player, Index) :-
    nl,
    (   Player = 'x'
    ->  format('--- Coup ~w | Joueur minimax ---~n', [Index])
    ;   format('--- Coup ~w | Joueur alpha_beta ---~n', [Index])
    ),
    display_board(Board),
    nl.

loop(Board, Player, Index) :-
    show_state(Board, Player, Index),
    (   game_over(Board, Result)
    ->  finalize(Result, Index)
    ;   (   Player = 'x'
        ->  apply_minimax(Board, NextBoard, Index, Time),
            format('Temps : ~3f s~n', [Time])
        ;   apply_alpha_beta(Board, NextBoard, Index, Time),
            format('Temps : ~3f s~n', [Time])
        ),
        change_player(Player, Next),
        NextIndex is Index + 1,
        loop(NextBoard, Next, NextIndex)
    ).

finalize('Draw', _) :-
    writeln('Resultat : egalite'),
    summary.

finalize(Winner, Count) :-
    format('Gagnant : ~w~n', [Winner]),
    format('Coups joues : ~w~n', [Count]),
    summary.

summary :-
    nl,
    collect(minimax_stats, MiniTimes),
    collect(alpha_beta_stats, ABTimes),
    report('minimax', MiniTimes),
    report('alpha_beta', ABTimes),
    compare(MiniTimes, ABTimes),
    nl.

collect(Pred, Times) :-
    findall(T, call(Pred, _, _, T), Times).

report(Label, Times) :-
    (   Times \= []
    ->  length(Times, N),
        sumlist(Times, Sum),
        Avg is Sum / N,
        max_member(Max, Times),
        min_member(Min, Times),
        format('~w -> nombre: ~w total: ~3f moy: ~3f min: ~3f max: ~3f~n',
               [Label, N, Sum, Avg, Min, Max])
    ;   format('~w -> pas de donnees~n', [Label])
    ).

compare(Mini, AB) :-
    (   Mini \= [], AB \= []
    ->  sumlist(Mini, M1),
        sumlist(AB, M2),
        (   M1 > M2
        ->  R is M1 / M2,
            format('alpha_beta plus rapide x~2f~n', [R])
        ;   R is M2 / M1,
            format('minimax plus rapide x~2f~n', [R])
        )
    ;   writeln('comparaison non disponible')
    ).

run_test :-
    retractall(minimax_stats(_, _, _)),
    retractall(alpha_beta_stats(_, _, _)),
    init_game,
    board(B),
    loop(B, 'x', 0).

:- run_test.
