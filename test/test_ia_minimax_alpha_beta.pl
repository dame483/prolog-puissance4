:- use_module(library(plunit)).

:- consult('../ia_minimax_alpha_beta.pl').

:- begin_tests(alpha_beta_all_tests).

% Test 1
test(alpha_beta_reaches_depth6) :-
    Board = [
        ['x','o'],
        ['o','x','o'],
        ['x','o','x'],
        ['o','x'],
        ['x'],
        ['o','x'],
        ['x']
    ],
        
    get_time(T1),
    ai_minimax_move(Board, 6, Col),
    get_time(T2),
    Time is T2 - T1,
    
    (Col = none ; between(1, 7, Col)),
    assertion(Time < 60),
    
    format('Profondeur 6 résolue en ~3f secondes avec coup ~w~n~n', [Time, Col]).

% Test 2
test(alpha_beta_multi_depth_comparison) :-
    Board = [
        ['x'],
        ['o','x'],
        ['x','o'],
        ['o','x','o'],
        ['x','o'],
        ['o','x'],
        ['x']
    ],
    
    Depths = [2, 4, 6],
    
    findall( depth_result(D, Move, Time), (member(D, Depths), get_time(T1), ai_minimax_move(Board, D, Move), get_time(T2), Time is T2 - T1), Results),
    
    forall(
        member(depth_result(D, M, T), Results),
        format('Depth: ~w, Move: ~w, Time: ~4f s~n', [D, M, T])
    ),
    
    assertion(length(Results, 3)).

:- end_tests(alpha_beta_all_tests).
