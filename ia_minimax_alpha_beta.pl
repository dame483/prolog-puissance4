:- consult(ia_naive). 
:- consult(evaluation). 
:- discontiguous minimax_ab/8.

utility_ab(Board, Value) :-
    evaluate(Board, 'x', ScoreX),
    evaluate(Board, 'o', ScoreO),
    Value is ScoreX - ScoreO.

valid_moves_ab(Board, Moves) :-
    findall(Col,( between(1, 7, Col), nth1(Col, Board, Column), length(Column, L), L < 6 ), Moves).

rank_columns(Columns, RankedColumns) :-
    partition(is_center_column, Columns, CenterCols, EdgeCols),
    append(CenterCols, EdgeCols, RankedColumns).

is_center_column(Col) :-
    member(Col, [3, 4, 5]).

% Victoire IA
minimax_ab(Board, _, _, _, _, _, none, 100000) :-
    win(Board, 'x'),
    !.

% Victoire adversaire
minimax_ab(Board, _, _, _, _, _, none, -100000) :-
    win(Board, 'o'),
    !.

% Profondeur maximale atteinte
minimax_ab(Board, Depth, MaxDepth, _, _, _, none, Value) :-
    Depth >= MaxDepth,
    utility_ab(Board, Value),
    !.

% Plateau plein
minimax_ab(Board, _, _, _, _, _, none, Value) :-
    isFullBoard(Board),
    utility_ab(Board, Value),
    !.

minimax_ab(Board, Depth, MaxDepth, Alpha, Beta, 'x', BestCol, BestValue) :-
    Depth < MaxDepth,
    valid_moves_ab(Board, Moves),
    Moves \= [],
    rank_columns(Moves, RankedMoves),
    max_value_ab(Board, RankedMoves, Depth, MaxDepth, Alpha, Beta, none, -100001, BestCol, BestValue).

max_value_ab(_, [], _, _, _, _, BestCol, BestVal, BestCol, BestVal) :-
    !.

max_value_ab(Board, [Col | Cols], Depth, MaxDepth,
          Alpha, Beta, CurrCol, CurrVal, BestCol, BestVal) :-

    D1 is Depth + 1,
    drop_token(Board, Col, 'x', NewBoard),
    minimax_ab(NewBoard, D1, MaxDepth, Alpha, Beta, 'o', _, Value),

    ( Value > CurrVal -> NewCol = Col, NewVal = Value; NewCol = CurrCol, NewVal = CurrVal ),

    NewAlpha is max(Alpha, NewVal),

    ( NewAlpha >= Beta -> BestCol = NewCol, BestVal = NewVal; max_value_ab(Board, Cols, Depth, MaxDepth, NewAlpha, Beta, NewCol, NewVal, BestCol, BestVal)).

minimax_ab(Board, Depth, MaxDepth, Alpha, Beta, 'o', BestCol, BestValue) :-
    Depth < MaxDepth,
    valid_moves_ab(Board, Moves),
    Moves \= [],
    rank_columns(Moves, RankedMoves),
    min_value_ab(Board, RankedMoves, Depth, MaxDepth, Alpha, Beta, none, 100001, BestCol, BestValue).

min_value_ab(_, [], _, _, _, _, BestCol, BestVal, BestCol, BestVal) :-
    !.

min_value_ab(Board, [Col | Cols], Depth, MaxDepth, Alpha, Beta, CurrCol, CurrVal, BestCol, BestVal) :-

    D1 is Depth + 1,
    drop_token(Board, Col, 'o', NewBoard),
    minimax_ab(NewBoard, D1, MaxDepth, Alpha, Beta, 'x', _, Value),

    ( Value < CurrVal -> NewCol = Col, NewVal = Value; NewCol = CurrCol, NewVal = CurrVal),

    NewBeta is min(Beta, NewVal),

    ( NewBeta =< Alpha -> BestCol = NewCol, BestVal = NewVal; min_value_ab(Board, Cols, Depth, MaxDepth, Alpha, NewBeta, NewCol, NewVal, BestCol, BestVal)).

ai_minimax_alpha_beta_move(Board, MaxDepth, Col) :-
    minimax_ab(Board, 0, MaxDepth, -100000, 100000, 'x', Col, _),
    !.