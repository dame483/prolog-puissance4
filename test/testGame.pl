test_Board([
    [],      
    [],      
    [],      
    [],      
    [],      
    [],      
    []      
]).

test_BoardFull([
    ['x','o','x','x','o','x','o'],      
    ['o','x','o','x','o','o','x'],      
    ['x','o','o','o','x','o','o'],      
    ['o','x','x','o','x','o','x'],      
    ['x','o','o','x','x','x','o'],     
    ['o','x','o','x','o','o','x'],      
    ['x','x','o','x','x','o','x']      
]).

% ?- isFullBoard(test_boardFull)