% Load necessary libraries
:- use_module(library(clpfd)).

% Predicate to read the entire Sudoku grid at once
read_sudoku(Grid) :-
    write('Enter the entire Sudoku grid as a nested list [[Row1], [Row2], ..., [Row9]] (use 0 for blanks): '),
    read(Term),
    (   is_valid_grid(Term) -> Grid = Term ; write('Invalid input, please enter a nested list of 9 rows, each with 9 numbers (0-9).\n'), read_sudoku(Grid) ).

% Validate the entire grid
is_valid_grid(Grid) :-
    is_list(Grid),
    length(Grid, 9),
    maplist(is_valid_row, Grid).

% Validate each row in the grid
is_valid_row(Row) :-
    is_list(Row),
    length(Row, 9),
    maplist(between(0, 9), Row).

% Entry point for solving a Sudoku puzzle with user input
start :-
    write('Enter your Sudoku puzzle as a single nested list.\n'),
    read_sudoku(Puzzle),
    (   sudoku(Puzzle, Solution) -> print_sudoku(Solution) ; write('No solution found.\n') ).

% Helper predicate to print the Sudoku board
print_sudoku([]).
print_sudoku([Head|Tail]) :-
    print_row(Head),
    nl,
    print_sudoku(Tail).

print_row([]).
print_row([Head|Tail]) :-
    write(Head),
    write(' '),
    print_row(Tail).

% Sudoku solver logic
sudoku(Puzzle, Solution) :-
    Solution = Puzzle,
    append(Puzzle, Vars),
    Vars ins 1..9,
    maplist(all_distinct, Puzzle),
    transpose(Puzzle, Columns),
    maplist(all_distinct, Columns),
    blocks(Puzzle).

% Helper to validate 3x3 sub-grids
blocks(Puzzle) :-
    Puzzle = [A,B,C,D,E,F,G,H,I],
    blocks(A, B, C),
    blocks(D, E, F),
    blocks(G, H, I).

% Validate a single block (3 rows)
blocks(Row1, Row2, Row3) :-
    blocks_row(Row1, Row2, Row3, 1),
    blocks_row(Row1, Row2, Row3, 4),
    blocks_row(Row1, Row2, Row3, 7).

% Validate a segment of 3x3 in the rows
blocks_row(R1, R2, R3, Index) :-
    nth1(Index, R1, A), nth1(Index+1, R1, B), nth1(Index+2, R1, C),
    nth1(Index, R2, D), nth1(Index+1, R2, E), nth1(Index+2, R2, F),
    nth1(Index, R3, G), nth1(Index+1, R3, H), nth1(Index+2, R3, I),
    all_distinct([A, B, C, D, E, F, G, H, I]).