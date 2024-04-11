:- use_module(utils, [valid_index/1]).

beginnerSudoku([cell(1, 1, 0), cell(1, 2, 0), cell(1, 3, 7), cell(1, 4, 0), cell(1, 5, 0), cell(1, 6, 9), cell(1, 7, 0), cell(1, 8, 4), cell(1, 9, 0), cell(2, 1, 2), cell(2, 2, 0), cell(2, 3, 0), cell(2, 4, 5), cell(2, 5, 7), cell(2, 6, 0), cell(2, 7, 0), cell(2, 8, 0), cell(2, 9, 0), cell(3, 1, 3), cell(3, 2, 8), cell(3, 3, 0), cell(3, 4, 0), cell(3, 5, 2), cell(3, 6, 0), cell(3, 7, 0), cell(3, 8, 7), cell(3, 9, 0), cell(4, 1, 1), cell(4, 2, 0), cell(4, 3, 3), cell(4, 4, 2), cell(4, 5, 4), cell(4, 6, 7), cell(4, 7, 0), cell(4, 8, 0), cell(4, 9, 6), cell(5, 1, 0), cell(5, 2, 6), cell(5, 3, 0), cell(5, 4, 8), cell(5, 5, 0), cell(5, 6, 3), cell(5, 7, 0), cell(5, 8, 9), cell(5, 9, 0), cell(6, 1, 8), cell(6, 2, 0), cell(6, 3, 0), cell(6, 4, 9), cell(6, 5, 6), cell(6, 6, 5), cell(6, 7, 7), cell(6, 8, 0), cell(6, 9, 3), cell(7, 1, 0), cell(7, 2, 3), cell(7, 3, 0), cell(7, 4, 0), cell(7, 5, 9), cell(7, 6, 0), cell(7, 7, 0), cell(7, 8, 2), cell(7, 9, 8), cell(8, 1, 0), cell(8, 2, 0), cell(8, 3, 0), cell(8, 4, 0), cell(8, 5, 8), cell(8, 6, 6), cell(8, 7, 0), cell(8, 8, 0), cell(8, 9, 1), cell(9, 1, 0), cell(9, 2, 1), cell(9, 3, 0), cell(9, 4, 4), cell(9, 5, 0), cell(9, 6, 0), cell(9, 7, 6), cell(9, 8, 0), cell(9, 9, 0)]).

% Ensuring all values in a row are unique
row_constraint(Row, Sudoku) :-
    findall(Value, member(cell(Row, _, Value), Sudoku), Values),
    exclude(=(0), Values, NonEmptyValues),
    sort(NonEmptyValues, SortedValues),
    length(NonEmptyValues, Len),
    length(SortedValues, Len).

% Ensuring all values in a column are unique
col_constraint(Col, Sudoku) :-
    findall(Value, member(cell(_, Col, Value), Sudoku), Values),
    exclude(=(0), Values, NonEmptyValues),
    sort(NonEmptyValues, SortedValues),
    length(NonEmptyValues, Len),
    length(SortedValues, Len).

% Ensuring all values in a 3x3 box are unique
box_constraint(Row, Col, Sudoku) :-
    RowStart is 1 + 3*((Row - 1) // 3),
    RowEnd is RowStart + 2,
    ColStart is 1 + 3*((Col - 1) // 3),
    ColEnd is ColStart + 2,
    findall(Value, (between(RowStart, RowEnd, R), between(ColStart, ColEnd, C), member(cell(R, C, Value), Sudoku)), Values),
    exclude(=(0), Values, NonEmptyValues),
    sort(NonEmptyValues, SortedValues),
    length(NonEmptyValues, Len),
    length(SortedValues, Len).

% Applying constraints to the entire Sudoku grid
apply_constraints(Sudoku) :-
    forall(valid_index(Row), row_constraint(Row, Sudoku)),
    forall(valid_index(Col), col_constraint(Col, Sudoku)),
    forall((valid_index(Row), valid_index(Col)), box_constraint(Row, Col, Sudoku)).

