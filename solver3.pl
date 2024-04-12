% Sudoku Solver using Forward Selection and MRV


% Predicate: solve_sudoku/2
% Description: Solves a Sudoku puzzle.
% Parameters:
%  - Puzzle: The Sudoku puzzle to solve.
%  - Solution: The solution to the Sudoku puzzle.
solve_sudoku(Puzzle, Solution) :-
    init_puzzle(Puzzle, InitState),
    solve_state(InitState, Solution).

% Predicate: init_puzzle/2
% Description: Initializes the Sudoku puzzle with possible values.
% Parameters:
%  - Puzzle: The Sudoku puzzle. 
%            Emtpy Sudoku: [].
%            Example Partially Filled Sudoku: [(1, 1, 1), (1, 2, 2), (1, 3, 3)]
%  - InitState: The initial state of the puzzle.
init_puzzle(Puzzle, InitState) :-
    writeln('Initializing Puzzle...'),
    findall(cell(Row, Col, Values), 
            ((between(1, 9, Row), between(1, 9, Col)),
             (member((Row, Col, Val), Puzzle) -> Values = [Val] ; possible_values(Values))),
            TempState),
    apply_forward_check(Puzzle, TempState, InitState).

% Predicate: apply_forward_check/3
% Description: Applies forward checking based on the initial puzzle constraints.
% Parameters:
%  - Puzzle: The Sudoku puzzle.
%  - CurrentState: The current state of the puzzle.
%  - FinalState: The final state of the puzzle.
apply_forward_check([], State, State).
apply_forward_check([(Row, Col, Val)|T], CurrentState, FinalState) :-
    format('Applying forward check for: ~w, ~w, ~w\n', [Row, Col, Val]),
    forward_check(Row, Col, Val, CurrentState, UpdatedState),
    apply_forward_check(T, UpdatedState, FinalState).
    % writeln('Forward check applied successfully.').

% Predicate: possible_values/1
% Description: Returns the possible values for any cell in the Sudoku puzzle.
% Parameters:
%  - Values: The possible values for a cell.
possible_values([1,2,3,4,5,6,7,8,9]).

% Predicate: solve_state/2
% Description: Solves the Sudoku puzzle from the current state.
% Parameters:
%  - Unsolved: The unsolved cells in the puzzle.
%  - Solution: The solution to the Sudoku puzzle.
solve_state([], []).
solve_state(Unsolved, [(Row, Col, Value) | Solution]) :-
    format('Solving cell: ~w, ~w\n', [Row, Col]),
    select_min_remaining_values_cell(Unsolved, cell(Row, Col, [Value]), RestCells),
    format('Selected Value: ~w for ~w, ~w\n', [Value, Row, Col]),
    forward_check(Row, Col, Value, RestCells, UpdatedRest),
    solve_state(UpdatedRest, Solution).

% Predicate: select_min_remaining_values_cell/3
% Description: Selects the cell with the minimum remaining values.
% Parameters:
%  - Cells: The cells in the puzzle.
%  - MinCell: The cell with the minimum remaining values.
%  - RestCells: The remaining cells after selecting the minimum cell.
select_min_remaining_values_cell(Cells, MinCell, RestCells) :-
    select(MinCell, Cells, RestCells),
    MinCell = cell(_, _, Values),
    length(Values, L),
    L > 1,
    \+ (member(cell(_, _, V), RestCells), length(V, L2), L2 < L, L2 > 1).

% Predicate: forward_check/5
% Description: Applies forward checking by eliminating the value from all peers.
% Parameters:
%  - Row: The row of the cell.
%  - Col: The column of the cell.
%  - Value: The value to eliminate.
%  - Cells: The cells in the puzzle.
%  - UpdatedCells: The cells after applying forward checking.
forward_check(Row, Col, Value, Cells, UpdatedCells) :-
    maplist(eliminate_value(Row, Col, Value), Cells, UpdatedCells).

% Predicate: eliminate_value/5
% Description: Eliminates the value from a cell.
% Parameters:
%  - Row: The row of the cell.
%  - Col: The column of the cell.
%  - Value: The value to eliminate.
%  - Cell: The cell to eliminate the value from.
%  - UpdatedCell: The cell after eliminating the value.
eliminate_value(_, _, _, cell(R, C, [V]), cell(R, C, [V])).
eliminate_value(Row, _, Value, cell(Row, Col, Values), cell(Row, Col, NewValues)) :-
    delete(Values, Value, NewValues), !.
eliminate_value(_, Col, Value, cell(Row, Col, Values), cell(Row, Col, NewValues)) :-
    delete(Values, Value, NewValues), !.
eliminate_value(Row, Col, Value, cell(R, C, Values), cell(R, C, NewValues)) :- 
    in_same_block(Row, Col, R, C),
    delete(Values, Value, NewValues), !.
eliminate_value(_, _, _, Cell, Cell).

% Predicate: in_same_block/4
% Description: Checks if two cells are in the same block.
% Parameters:
%  - Row1: The row of the first cell.
%  - Col1: The column of the first cell.
%  - Row2: The row of the second cell.
%  - Col2: The column of the second cell.
in_same_block(Row1, Col1, Row2, Col2) :-
    ((Row1 - 1) // 3) == ((Row2 - 1) // 3),
    ((Col1 - 1) // 3) == ((Col2 - 1) // 3).


% Test Cases

% Test case for solve_sudoku/2

% Test case for init_puzzle/2
% ?- init_puzzle([], InitState).
%    All cells in InitState should have possible values [1,2,3,4,5,6,7,8,9].
% ?- init_puzzle([(1, 1, 1), (1, 2, 2), (1, 3, 3)], InitState).
%    All cells in InitState should have possible values [1,2,3,4,5,6,7,8,9] 
%    except for cell(1, 1, [1]), cell(1, 2, [2]), cell(1, 3, [3]),
%    and for cells that have been pruned becuase of the row, column, or box constraints.

% Test case for apply_forward_check/3
% ?- apply_forward_check([], [], FinalState).
%    FinalState should be [].
% ?- apply_forward_check(
%        [(1, 1, 1)], 
%        [cell(1, 1, [1]), cell(1, 2, [1,2,3]), cell(2, 1, [1,2,3]), cell(6, 7, [1,2,3])], 
%        FinalState
%    ).
%    FinalState should be [cell(1, 1, [1]), cell(1, 2, [2,3]), cell(2, 1, [2,3]), cell(6, 7, [1,2,3])].
%    1 should be pruned from cell(1, 2, [1,2,3]) and cell(2, 1, [1,2,3]).
