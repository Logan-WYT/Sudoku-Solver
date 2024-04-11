:- module(display, [display_board/1]).

display_board(Board) :-
    findall(Value, member(cell(1, _, Value), Board), R1),
    findall(Value, member(cell(2, _, Value), Board), R2),
    findall(Value, member(cell(3, _, Value), Board), R3),
    findall(Value, member(cell(4, _, Value), Board), R4),
    findall(Value, member(cell(5, _, Value), Board), R5),
    findall(Value, member(cell(6, _, Value), Board), R6),
    findall(Value, member(cell(7, _, Value), Board), R7),
    findall(Value, member(cell(8, _, Value), Board), R8),
    findall(Value, member(cell(9, _, Value), Board), R9),
    
    printHorizontal,

    display_helper([R1, R2, R3, R4, R5, R6, R7, R8, R9]),

    printHorizontal.

display_helper([]).
display_helper([Row | Rows]) :-
    displayRow(Row),
    display_helper(Rows).

displayRow([E1, E2, E3, E4, E5, E6, E7, E8, E9]) :-
    write('| '),
    write(E1),
    write(' | '),
    write(E2),
    write(' | '),
    write(E3),
    write(' | '),
    write(E4),
    write(' | '), 
    write(E5),
    write(' | '),
    write(E6),
    write(' | '),
    write(E7),
    write(' | '),
    write(E8),
    write(' | '),
    write(E9),
    write(' |\n').

printHorizontal :-
    write('+---+---+---+---+---+---+---+---+---+\n').
