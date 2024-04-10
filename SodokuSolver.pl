% Representing the Sudoku grid
valid_index(I) :- member(I, [1,2,3,4,5,6,7,8,9]).

% Representing the cells and ensuring the values are between 1 and 9
cell(Row, Col, Value) :- valid_index(Row), valid_index(Col), member(Value, [0,1,2,3,4,5,6,7,8,9]).

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