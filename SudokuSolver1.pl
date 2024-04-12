:- use_module(library(clpfd)).
:- [display4].
:- [boards4].


% Sudoku Solver

% Predicate: sudoku/1
% Description: Solves a Sudoku puzzle represented by a 9x9 grid.
% Parameters:
%   - Board: The Sudoku grid represented as a nested list.
sudoku(Board) :-
    flatten(Board, FlattenBoard), FlattenBoard ins 1..9, % Assert that the values are between 1 and 9
    row_constraint(Board), 
    column_constraint(Board), 
    box_constraint(Board).

% Predicate: row_constraint/1
% Description: Ensures that each row in the Sudoku grid contains distinct values.
% Parameters:
%   - Board: The Sudoku grid represented as a nested list.
row_constraint(Board) :-
    maplist(all_distinct, Board).

% Predicate: column_constraint/1
% Description: Ensures that each column in the Sudoku grid contains distinct values.
% Parameters:
%   - Board: The Sudoku grid represented as a nested list.
column_constraint(Board) :-
    transpose(Board, Columns),
    maplist(all_distinct, Columns).

% Predicate: box_constraint/1
% Description: Ensures that each 3x3 box in the Sudoku grid contains distinct values.
% Parameters:
%   - Board: The Sudoku grid represented as a nested list.
box_constraint(Board) :-
    Board = [Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9],
    box_constraint_helper(Row1, Row2, Row3), 
    box_constraint_helper(Row4, Row5, Row6), 
    box_constraint_helper(Row7, Row8, Row9).

% Predicate: box_constraint_helper/3
% Description: Helper predicate for box_constraint/1. Ensures that each 3x3 box contains distinct values.
% Parameters:
%   - Box1, Box2, Box3: The first row of the box.
%   - Box4, Box5, Box6: The second row of the box.
%   - Box7, Box8, Box9: The third row of the box.
box_constraint_helper([], [], []).
box_constraint_helper([Box1, Box2, Box3|Rest1], [Box4,Box5,Box6|Rest2], [Box7,Box8,Box9|Rest3]) :-
    all_distinct([Box1,Box2,Box3,Box4,Box5,Box6,Box7,Box8,Box9]),
    box_constraint_helper(Rest1, Rest2, Rest3).

% Predicate: flatten/2
% Description: Flattens a nested list into a single list.
% Parameters:
%   - Board: The nested list to be flattened.
%   - FlattenBoard: The resulting flattened list.
flatten(Board, FlattenBoard) :-
    append(Board, FlattenBoard).

% Predicate: solve/1
% Description: Solves a Sudoku puzzle and displays the solution.
% Parameters:
%   - Board: The Sudoku grid represented as a nested list.
solve(Board) :-
    sudoku(Board),
    display_sudoku(Board).

% Predicate: main/0
% Description: Entry point of the program. Allows the user to choose a Sudoku board and solves it.
main :-
    write('Choose a sample sudoku board (easy, medium, hard): '),
    read(BoardDifficulty),
    board(BoardDifficulty, Board),
    ( member(BoardDifficulty, [easy, medium, hard])
    -> write('Invalid problem number. Exiting.'), halt
    ; write('Solving Sudoku...\n'), 
    solve(Board),
    write('Solution complete.'),
    halt
    ).

% Predicate: main2/0
% Description: Entry point of the program. Allows the user to enter a custom Sudoku board and solves it.
main2 :-
    write('Enter the entire Sudoku grid as a nested list [[Row1], [Row2], ..., [Row9]] (use _ for blanks): '),
    read(Board),
    solve(Board).

% Test Cases

% Test case for row_constraint/1
% A valid row with distinct values
% ?- row_constraint([[1, 2, 3, 4, 5, 6, 7, 8, 9]]).
% ?- row_constraint([[9, 8, 7, 6, 5, 4, 3, 2, 1]]).
% ?- row_constraint([[1, 3, 5, 6, 2, 9, 4, 8, 7]]).
% An invalid row with duplicate values
% ?- \+ row_constraint([[1, 1, 1, 1, 1, 1, 1, 1, 1]]).
% ?- \+ row_constraint([[1, 2, 3, 4, 5, 6, 7, 8, 1]]).

% Test case for column_constraint/1
% A valid column with distinct values
% ?- column_constraint([[1], [2], [3], [4], [5], [6], [7], [8], [9]]).
% ?- column_constraint([[9], [8], [7], [6], [5], [4], [3], [2], [1]]).
% ?- column_constraint([[1], [3], [5], [6], [2], [9], [4], [8], [7]]).
% An invalid column with duplicate values
% ?- \+ column_constraint([[1], [1], [1], [1], [1], [1], [1], [1], [1]]).
% ?- \+ column_constraint([[1], [2], [3], [4], [5], [6], [7], [8], [1]]).

% Test case for box_constraint/1
% A valid box with distinct values
% ?- box_constraint([
%     [5, 3, 4, 6, 7, 8, 9, 1, 2],
%     [6, 7, 2, 1, 9, 5, 3, 4, 8],
%     [1, 9, 8, 3, 4, 2, 5, 6, 7],
%     [8, 5, 9, 7, 6, 1, 4, 2, 3],
%     [4, 2, 6, 8, 5, 3, 7, 9, 1],
%     [7, 1, 3, 9, 2, 4, 8, 5, 6],
%     [9, 6, 1, 5, 3, 7, 2, 8, 4],
%     [2, 8, 7, 4, 1, 9, 6, 3, 5],
%     [3, 4, 5, 2, 8, 6, 1, 7, 9]
% ]).
% ?- box_constraint([
%     [1, 2, 3, 4, 5, 6, 7, 8, 9],
%     [4, 5, 6, 7, 8, 9, 1, 2, 3],
%     [7, 8, 9, 1, 2, 3, 4, 5, 6],
%     [2, 1, 4, 3, 6, 5, 8, 9, 7],
%     [3, 6, 5, 8, 9, 7, 2, 1, 4],
%     [8, 9, 7, 2, 1, 4, 3, 6, 5],
%     [5, 3, 1, 6, 4, 2, 9, 7, 8],
%     [6, 4, 2, 9, 7, 8, 5, 3, 1],
%     [9, 7, 8, 5, 3, 1, 6, 4, 2]
% ]).
% An invalid box with duplicate values
% ?- \+ box_constraint([[1, 2, 3], [4, 5, 6], [7, 8, 1]]).

% Test case for flatten/2
% Flattening a nested list
% ?- flatten([[],[]], []).
% ?- flatten([[1]], [1]).
% ?- flatten([[1, 2, 3], [4, 5, 6], [7, 8, 9]], [1, 2, 3, 4, 5, 6, 7, 8, 9]).

% Test case for solve/1
% Solving a Sudoku puzzle
% ?- solve([[5, 3, _, _, 7, _, _, _, _],
%           [6, _, _, 1, 9, 5, _, _, _],
%           [_, 9, 8, _, _, _, _, 6, _],
%           [8, _, _, _, 6, _, _, _, 3],
%           [4, _, _, 8, _, 3, _, _, 1],
%           [7, _, _, _, 2, _, _, _, 6],
%           [_, 6, _, _, _, _, 2, 8, _],
%           [_, _, _, 4, 1, 9, _, _, 5],
%           [_, _, _, _, 8, _, _, 7, 9]]).
% ?- board(easy, Board), solve(Board).
% ?- board(medium, Board), solve(Board).
% ?- board(hard, Board), solve(Board).
