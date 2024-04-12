% Entry point to display a Sudoku board with subgrid lines.
display_sudoku(Board) :-
  maplist_with_index(display_row, Board).

% Helper predicate to apply a predicate with an index.
maplist_with_index(Pred, List) :-
  maplist_with_index(Pred, List, 1).

maplist_with_index(_, [], _).
maplist_with_index(Pred, [H|T], Index) :-
  call(Pred, Index, H),
  NextIndex is Index + 1,
  maplist_with_index(Pred, T, NextIndex).

% Display a single row of the Sudoku board, adding subgrid lines as needed.
display_row(Index, Row) :-
  (Index == 1; Index == 4; Index == 7),
  print_horizontal,
  display_row_cells(Row),
  nl.
display_row(Index, Row) :-
  display_row_cells(Row),
  (Index == 9 -> nl, print_horizontal ; true),
  nl.

% Display the cells of a single row, including vertical separators.
display_row_cells(Row) :-
  maplist_with_index(display_cell, Row).

% Display a single cell with vertical separators as needed.
display_cell(Index, Cell) :-
  (Index == 1; Index == 4; Index == 7),
  write('|'),
  display_cell_content(Cell).
display_cell(Index, Cell) :-
  display_cell_content(Cell),
  (Index == 9 -> write('|') ; true).

% Display the content of a single cell, replacing variables with spaces.
display_cell_content(Cell) :-
  (   var(Cell)  % Check if Cell is an unbound variable
  ->  write(' _ ')  % Print an underscore for an empty cell
  ;   write(' '), write(Cell), write(' ')  % Print the cell value
  ).

print_horizontal :-
    writeln('+---------+---------+---------+').