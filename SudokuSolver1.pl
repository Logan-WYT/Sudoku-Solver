:- use_module(library(clpfd)).
:- [display4].
:- [boards4].

sudoku(Board) :-
    flatten(Board, FlattenBoard), FlattenBoard ins 1..9, % Assert that the values are between 1 and 9
    row_constraint(Board), 
    column_constraint(Board), 
    box_constraint(Board).

row_constraint(Board) :-
    maplist(all_distinct, Board).

column_constraint(Board) :-
    transpose(Board, Columns),
    maplist(all_distinct, Columns).

box_constraint(Board) :-
    Board = [Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9],
    box_constraint_helper(Row1, Row2, Row3), 
    box_constraint_helper(Row4, Row5, Row6), 
    box_constraint_helper(Row7, Row8, Row9).
box_constraint_helper([], [], []).
box_constraint_helper([Box1, Box2, Box3|Rest1], [Box4,Box5,Box6|Rest2], [Box7,Box8,Box9|Rest3]) :-
    all_distinct([Box1,Box2,Box3,Box4,Box5,Box6,Box7,Box8,Box9]),
    box_constraint_helper(Rest1, Rest2, Rest3).

flatten(Board, FlattenBoard) :-
    append(Board, FlattenBoard).

solve(Board) :-
    sudoku(Board),
    display_sudoku(Board).

main :-
    write('Choose a sample sudoku board (easy, medium, hard): '),
    read(BoardDifficculty),
    board(BoardDifficculty, Board),
    ( BoardDifficculty = easy, BoardDifficculty = medium, BoardDifficculty = hard
    -> write('Invalid problem number. Exiting.'), halt
    ; write('Solving Sudoku...\n'),
    solve(Board),
    write('Solution complete.'),
    halt
    ).
