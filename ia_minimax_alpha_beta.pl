:- consult('ia_naive.pl').
:- consult('evaluation.pl').
:- consult('game.pl').

/* ===========================
   JOUEURS
   =========================== */

max_player('x').    % IA
min_player('o').    % Adversaire

/* ===========================
   FONCTION D’UTILITÉ
   =========================== */

utility(Board, Value) :-
    evaluate(Board, 'x', ScoreX),
    evaluate(Board, 'o', ScoreO),
    Value is ScoreX - ScoreO.

/* ===========================
   COUPS VALIDES
   =========================== */

valid_moves(Board, Moves) :-
    findall(Col,
        ( between(1, 7, Col),
          nth1(Col, Board, Column),
          length(Column, L),
          L < 6 ),
        Moves).

/* ===========================
   ORDRE DES COUPS (CENTRE)
   =========================== */

rank_columns(Columns, RankedColumns) :-
    partition(is_center_column, Columns, CenterCols, EdgeCols),
    append(CenterCols, EdgeCols, RankedColumns).

is_center_column(Col) :-
    member(Col, [3, 4, 5]).

/* ===========================
   MINIMAX ALPHA-BETA
   =========================== */

% --- Victoire IA ---
minimax_ab(Board, _, _, _, _, _, none, 100000) :-
    win(Board, 'x'),
    !.

% --- Victoire adversaire ---
minimax_ab(Board, _, _, _, _, _, none, -100000) :-
    win(Board, 'o'),
    !.

% --- Profondeur maximale atteinte ---
minimax_ab(Board, Depth, MaxDepth, _, _, _, none, Value) :-
    Depth >= MaxDepth,
    utility(Board, Value),
    !.

% --- Plateau plein ---
minimax_ab(Board, _, _, _, _, _, none, Value) :-
    isFullBoard(Board),
    utility(Board, Value),
    !.

/* ===========================
   NŒUD MAX (IA : x)
   =========================== */

minimax_ab(Board, Depth, MaxDepth, Alpha, Beta, 'x', BestCol, BestValue) :-
    Depth < MaxDepth,
    valid_moves(Board, Moves),
    Moves \= [],
    rank_columns(Moves, RankedMoves),
    max_value(Board, RankedMoves, Depth, MaxDepth,
              Alpha, Beta, none, -100001, BestCol, BestValue).

max_value(_, [], _, _, _, _, BestCol, BestVal, BestCol, BestVal) :-
    !.

max_value(Board, [Col | Cols], Depth, MaxDepth,
          Alpha, Beta, CurrCol, CurrVal, BestCol, BestVal) :-

    D1 is Depth + 1,
    drop_token(Board, Col, 'x', NewBoard),
    minimax_ab(NewBoard, D1, MaxDepth, Alpha, Beta, 'o', _, Value),

    ( Value > CurrVal ->
        NewCol = Col,
        NewVal = Value
    ;
        NewCol = CurrCol,
        NewVal = CurrVal
    ),

    NewAlpha is max(Alpha, NewVal),

    ( NewAlpha >= Beta ->
        BestCol = NewCol,
        BestVal = NewVal
    ;
        max_value(Board, Cols, Depth, MaxDepth,
                  NewAlpha, Beta, NewCol, NewVal, BestCol, BestVal)
    ).

/* ===========================
   NŒUD MIN (ADVERSAIRE : o)
   =========================== */

minimax_ab(Board, Depth, MaxDepth, Alpha, Beta, 'o', BestCol, BestValue) :-
    Depth < MaxDepth,
    valid_moves(Board, Moves),
    Moves \= [],
    rank_columns(Moves, RankedMoves),
    min_value(Board, RankedMoves, Depth, MaxDepth,
              Alpha, Beta, none, 100001, BestCol, BestValue).

min_value(_, [], _, _, _, _, BestCol, BestVal, BestCol, BestVal) :-
    !.

min_value(Board, [Col | Cols], Depth, MaxDepth,
          Alpha, Beta, CurrCol, CurrVal, BestCol, BestVal) :-

    D1 is Depth + 1,
    drop_token(Board, Col, 'o', NewBoard),
    minimax_ab(NewBoard, D1, MaxDepth, Alpha, Beta, 'x', _, Value),

    ( Value < CurrVal ->
        NewCol = Col,
        NewVal = Value
    ;
        NewCol = CurrCol,
        NewVal = CurrVal
    ),

    NewBeta is min(Beta, NewVal),

    ( NewBeta =< Alpha ->
        BestCol = NewCol,
        BestVal = NewVal
    ;
        min_value(Board, Cols, Depth, MaxDepth,
                  Alpha, NewBeta, NewCol, NewVal, BestCol, BestVal)
    ).

/* ===========================
   INTERFACE PUBLIQUE
   =========================== */

ai_minimax_move(Board, MaxDepth, Col) :-
    minimax_ab(Board, 0, MaxDepth, -100000, 100000, 'x', Col, _),
    !.
