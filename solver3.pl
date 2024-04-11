% Solve the puzzle
solve_sudoku(Puzzle, Solution) :-
    init_puzzle(Puzzle, InitState),
    solve_state(InitState, Solution).

% Initialize puzzle with possible values
init_puzzle(Puzzle, InitState) :-
    writeln('Initializing Puzzle...'),
    findall(cell(Row, Col, Values), 
            (between(1, 9, Row), 
             between(1, 9, Col), 
             (member((Row, Col, Val), Puzzle) -> Values = [Val] ; possible_values(Values))),
            TempState),
    apply_forward_check(Puzzle, TempState, InitState).

% Apply forward checking based on the initial puzzle constraints
apply_forward_check([], State, State).
apply_forward_check([(Row, Col, Val)|T], CurrentState, FinalState) :-
    format('Applying forward check for: ~w, ~w, ~w\n', [Row, Col, Val]),
    forward_check(Row, Col, Val, CurrentState, UpdatedState),
    apply_forward_check(T, UpdatedState, FinalState).
    % writeln('Forward check applied successfully.').

% Possible values for any cell
possible_values([1,2,3,4,5,6,7,8,9]).

% Solve from current state
solve_state([], []).
solve_state(Unsolved, [(Row, Col, Value) | Solution]) :-
    format('Solving cell: ~w, ~w\n', [Row, Col]),
    select_min_remaining_values_cell(Unsolved, cell(Row, Col, [Value]), RestCells),
    format('Selected Value: ~w for ~w, ~w\n', [Value, Row, Col]),
    forward_check(Row, Col, Value, RestCells, UpdatedRest),
    solve_state(UpdatedRest, Solution).

% Select the cell with the minimum remaining values
select_min_remaining_values_cell(Cells, MinCell, RestCells) :-
    select(MinCell, Cells, RestCells),
    MinCell = cell(_, _, Values),
    length(Values, L),
    L > 1,
    \+ (member(cell(_, _, V), RestCells), length(V, L2), L2 < L, L2 > 1).
    writeln('Selected cell with minimum remaining values. Row: ~w, Col: ~w, Value: ~w', [R,C,Values]).


% Forward checking: eliminate Value from all peers
forward_check(Row, Col, Value, Cells, UpdatedCells) :-
    maplist(eliminate_value(Row, Col, Value), Cells, UpdatedCells).

eliminate_value(_, _, _, cell(R, C, [V]), cell(R, C, [V])).
eliminate_value(Row, _, Value, cell(Row, Col, Values), cell(Row, Col, NewValues)) :-
    delete(Values, Value, NewValues), !.
eliminate_value(_, Col, Value, cell(Row, Col, Values), cell(Row, Col, NewValues)) :-
    delete(Values, Value, NewValues), !.
eliminate_value(Row, Col, Value, cell(R, C, Values), cell(R, C, NewValues)) :- 
    in_same_block(Row, Col, R, C),
    delete(Values, Value, NewValues) !.
eliminate_value(_, _, _, Cell, Cell).

in_same_block(Row1, Col1, Row2, Col2) :-
    ((Row1 - 1) // 3) == ((Row2 - 1) // 3),
    ((Col1 - 1) // 3) == ((Col2 - 1) // 3).