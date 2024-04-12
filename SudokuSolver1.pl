:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(format)).

% Read and Validate Sudoku Grid from User Input
read_sudoku(Grid) :-
    write('Enter the entire Sudoku grid as a nested list [[Row1], [Row2], ..., [Row9]] (use 0 for blanks): '),
    read(Term),
    (   is_valid_grid(Term) -> Grid = Term ; write('Invalid input, please enter a nested list of 9 rows, each with 9 numbers (0-9).\\n'), read_sudoku(Grid) ).

is_valid_grid(Grid) :-
    length(Grid, 9),
    maplist(valid_row, Grid).

valid_row(Row) :-
    length(Row, 9),
    maplist(between(0, 9), Row).


% Constraint Posting for Sudoku
sudoku(Rows) :-
    length(Rows, 9), maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns), maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs), blocks(Ds, Es, Fs), blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).


%  Main Execution and Display Function
show(Options, Rows) :-
    sudoku(Rows),
    maplist(labeling(Options), Rows),
    maplist(print_row, Rows).

print_row(Row) :-
    maplist(print_element, Row),
    nl.

print_element(Elem) :-
    format(" ~w ", [Elem]).




% Sample problem
problem(1, P) :- % Beginner level example
P = [[1,,,8,,4,,,],
[,2,,,,,4,5,6],
[,,3,2,,5,,,],
[,,,4,,,8,,5],
[7,8,9,,5,,,,],
[,,,,,6,2,,3],
[8,,1,,,,7,,],
[,,,1,2,3,,8,],
[2,,5,,,,,,9]].

problem(2, P) :- % Intermediate level example
P = [[,,2,,3,,1,,],
[,4,,,,,,3,],
[1,,5,,,,,8,2],
[,,,2,,,6,5,],
[9,,,,8,7,,,3],
[,,,,4,,,,],
[8,,,,7,,,,4],
[,9,3,1,,,,6,],
[,,7,,6,,5,,]].


:- initialization(main).

main :-
write('Choose a sample problem (1 or 2): '),
read(ProblemId),
problem(ProblemId, Grid),
( ProblemId = 1, ProblemId = 2
-> write('Invalid problem number. Exiting.'), halt
; write('Solving Sudoku...\n'),
show([ff], Grid),
write('Solution complete.'),
halt
).