:- module(utils, [valid_index/1, cell/3]).

% Representing the Sudoku grid
valid_index(I) :- member(I, [1,2,3,4,5,6,7,8,9]).

% Representing the cells and ensuring the values are between 1 and 9
cell(Row, Col, Value) :- valid_index(Row), valid_index(Col), member(Value, [0,1,2,3,4,5,6,7,8,9]).
