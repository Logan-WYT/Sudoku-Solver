:- use_module(display, [display_board/1]).

%% Define the size of the Sudoku.
size(9).
block_size(3).

%% Possible values for any cell.
possible_values([1, 2, 3, 4, 5, 6, 7, 8, 9]).

%% Initialize a cell with all possible values.
init_cell(Cell) :-
    possible_values(Possibilities),
    Cell = cell(_, _, Possibilities).

%% Set up the initial state of the Sudoku grid.
initialize([], []).
initialize([cell(R,C,_) | Rest], [cell(R,C,Values) | State]) :-
    possible_values(Values),
    initialize(Rest, State).

%% Solving the Sudoku.
solve(Sudoku, Solved) :-
    initial_cells(Cells),
    initialize(Cells, Sudoku),
    solve_sudoku(Sudoku, Solved).

%% Helper for generating row and column indices.
initial_cells(Cells) :-
    findall(cell(R,C, _),
            (between(1, 9, R), between(1, 9, C)),
            Cells).

%% The main backtracking solver function
solve_sudoku([], []).
solve_sudoku([cell(Row, Col, Values) | Rest], [cell(Row, Col, Value) | Solution]) :-
    member(Value, Values),
    valid_choice(Row, Col, Value, Rest),
    apply_choice(Row, Col, Value, Rest, NewRest),
    solve_sudoku(NewRest, Solution).

%% Validate choice does not violate Sudoku's rules
valid_choice(Row, Col, Value, Cells) :-
    not(member(cell(Row, _, Value), Cells)),
    not(member(cell(_, Col, Value), Cells)),
    RowStart is 1 + (Row - 1) // 3 * 3,
    ColStart is 1 + (Col - 1) // 3 * 3,
    RowEnd is RowStart + 2,
    ColEnd is ColStart + 2,
    findall(V, (between(RowStart, RowEnd, R),
                between(ColStart, ColEnd, C),
                member(cell(R, C, V), Cells)), Block),
    not(member(Value, Block)).

%% Apply a choice by removing the value from potential values of all related cells.
% apply_choice(Row, Col, Value, Cells, NewCells) :-
%     maplist(remove_value(Row, Col, Value), Cells, NewCells).

apply_choice(Row, Col, Value, Cells, NewCells) :-
    block_size(BSize),
    RowStart is ((Row - 1) div BSize) * BSize + 1,
    ColStart is ((Col - 1) div BSize) * BSize + 1,
    maplist(remove_value(Row, Col, Value, RowStart, ColStart, BSize), Cells, NewCells).

remove_value(Row, Col, Value, RowStart, ColStart, BSize, cell(R, C, Values), cell(R, C, NewValues)) :-
    (R = Row; C = Col; (R >= RowStart, R < RowStart + BSize, C >= ColStart, C < ColStart + BSize)),
    !, delete(Values, Value, NewValues).
remove_value(_, _, _, _, _, _, Cell, Cell).

%% Example of starting the solver
?- initial_cells(Cells), initialize(Cells, Sudoku), solve(Sudoku, Solution), display_board(Solution).
