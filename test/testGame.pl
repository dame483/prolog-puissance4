test_board([
    ['x','o','x'],      
    ['x','o'],      
    ['x'],      
    ['x'],      
    ['o','o','o'],      
    [],      
    []      
]).

test_board2([
    ['x','o','x'],      
    ['o','o'],      
    ['x','x'],      
    ['x'],      
    ['o','o','o','o'],      
    [],      
    []      
]).

test_board3([
    ['x','o','x','x'],      
    ['o','o','x','x'],      
    ['x','x','o'],      
    ['x','o'],      
    ['o','x','o','o'],      
    [],      
    []      
]).

test_board4([
    ['x','o','x','o'],      
    ['o','o','x','x'],      
    ['x','x','o'],      
    ['x','o','x','o'],      
    ['o','x','o','o','o'],      
    [],      
    []      
]).

test_board5([
    ['x','o','x','o'],      
    ['o','o','x','x'],      
    ['x','x','o'],      
    ['x','o','x','x'],      
    ['o','x','o','o','o'],      
    [],      
    []      
]).

test_board6([
    ['x','o','x','o','x','o'],      
    ['o','x','o','x','o','x'],      
    ['x','o','x','o','x','o'],      
    ['x','o','x','o','x','o'],      
    ['o','x','o','x','o','x'],      
    ['x','o','x','o','x','o'],      
    ['x','o','x','o','x','o']
]).

% ?- test_board4(B), diagonal_win_down(B, 'o').
% ?- test_board3(B), diagonal_win_up(B, 'x').
% ?- test_board2(B), vertical_win(B,'o').
% ?- test_board2(B), horizontal_win(B, 'x').

% ?- test_board3(B), game_over(B,Winner).
% ?- test_board4(B), game_over(B,Winner).
% ?- test_board5(B), game_over(B,Winner).