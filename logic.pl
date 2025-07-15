:- consult('facts.pl').
:- use_module(library(http/json)).

validate_grid(Grid) :-
    update_grid_from_json(Grid),
    validate_all_rules.

update_grid_from_json(GridData) :-
    retractall(cell(_, _, _, _, _)),
    assert_grid_from_json(GridData, 1).

assert_grid_from_json([], _).
assert_grid_from_json([Row|Rest], R) :-
    assert_row_from_json(Row, R, 1),
    R1 is R + 1,
    assert_grid_from_json(Rest, R1).

assert_row_from_json([], _, _).
assert_row_from_json([Cell|Rest], R, C) :-
    (   Cell == 2 -> Type = ship ; Type = sea ),
    assert(cell(R, C, Type, nil, nil)),
    C1 is C + 1,
    assert_row_from_json(Rest, R, C1).

validate_all_rules :-
    validate_row_counts,
    validate_col_counts,
    validate_no_diagonal_touching,
    validate_ship_shapes,
    validate_ship_counts.

validate_row_counts :-
    grid(MaxRows),
    forall(between(1, MaxRows, R), (
        row(R, Expected),
        count_ships_in_row(R, Count),
        Expected =:= Count
    )).

validate_col_counts :-
    grid(MaxCols),
    forall(between(1, MaxCols, C), (
        col(C, Expected),
        count_ships_in_col(C, Count),
        Expected =:= Count
    )).

validate_no_diagonal_touching :-
    grid(Size),
    forall(between(1, Size, R), 
        forall(between(1, Size, C), 
            (cell(R, C, ship, _, _) -> 
                \+ has_diagonal_ship_neighbor(R, C) ; true))).

validate_ship_shapes :-
    find_all_ships(Ships),
    forall(member(Ship, Ships), validate_ship_shape(Ship)).

validate_ship_counts :-
    find_all_ships(Ships),
    group_ships_by_length(Ships, GroupedShips),
    forall(ship(Length, ExpectedCount), (
        (   member(Length-ActualCount, GroupedShips) ->
            ActualCount =:= ExpectedCount
        ;   ExpectedCount =:= 0
        )
    )).

count_ships_in_row(R, Count) :-
    grid(MaxCols),
    findall(1, (between(1, MaxCols, C), cell(R, C, ship, _, _)), L),
    length(L, Count).

count_ships_in_col(C, Count) :-
    grid(MaxRows),
    findall(1, (between(1, MaxRows, R), cell(R, C, ship, _, _)), L),
    length(L, Count).

has_diagonal_ship_neighbor(R, C) :-
    DiagonalOffsets = [(-1,-1), (-1,1), (1,-1), (1,1)],
    member((DR, DC), DiagonalOffsets),
    NR is R + DR,
    NC is C + DC,
    grid(Size),
    NR >= 1, NR =< Size,
    NC >= 1, NC =< Size,
    cell(NR, NC, ship, _, _).

find_all_ships(Ships) :-
    grid(Size),
    findall(Ship, find_ship_starting_at(Size, Ship), AllShips),
    sort(AllShips, Ships).

find_ship_starting_at(Size, Ship) :-
    between(1, Size, R),
    between(1, Size, C),
    cell(R, C, ship, _, _),
    \+ has_ship_neighbor_before(R, C),
    build_ship_from(R, C, Ship).

has_ship_neighbor_before(R, C) :-
    (   R > 1, cell(R-1, C, ship, _, _)
    ;   C > 1, cell(R, C-1, ship, _, _)
    ).

build_ship_from(R, C, Ship) :-
    (   has_horizontal_continuation(R, C) ->
        build_horizontal_ship(R, C, Ship)
    ;   has_vertical_continuation(R, C) ->
        build_vertical_ship(R, C, Ship)
    ;   Ship = [(R, C)]
    ).

has_horizontal_continuation(R, C) :-
    grid(Size),
    C1 is C + 1,
    C1 =< Size,
    cell(R, C1, ship, _, _).

has_vertical_continuation(R, C) :-
    grid(Size),
    R1 is R + 1,
    R1 =< Size,
    cell(R1, C, ship, _, _).

build_horizontal_ship(R, C, Ship) :-
    build_horizontal_ship_acc(R, C, [], Ship).

build_horizontal_ship_acc(R, C, Acc, Ship) :-
    (   cell(R, C, ship, _, _) ->
        NewAcc = [(R, C)|Acc],
        C1 is C + 1,
        build_horizontal_ship_acc(R, C1, NewAcc, Ship)
    ;   reverse(Acc, Ship)
    ).

build_vertical_ship(R, C, Ship) :-
    build_vertical_ship_acc(R, C, [], Ship).

build_vertical_ship_acc(R, C, Acc, Ship) :-
    (   cell(R, C, ship, _, _) ->
        NewAcc = [(R, C)|Acc],
        R1 is R + 1,
        build_vertical_ship_acc(R1, C, NewAcc, Ship)
    ;   reverse(Acc, Ship)
    ).

validate_ship_shape(Ship) :-
    length(Ship, Length),
    (   Length =:= 1 -> true
    ;   Length > 1 -> 
        (   is_horizontal_ship(Ship) ; is_vertical_ship(Ship))
    ).

is_horizontal_ship([(R, C1), (R, C2)|Rest]) :-
    C2 =:= C1 + 1,
    is_horizontal_ship([(R, C2)|Rest]).
is_horizontal_ship([_]).

is_vertical_ship([(R1, C), (R2, C)|Rest]) :-
    R2 =:= R1 + 1,
    is_vertical_ship([(R2, C)|Rest]).
is_vertical_ship([_]).

group_ships_by_length(Ships, GroupedShips) :-
    findall(Length-Count, (
        findall(L, (member(Ship, Ships), length(Ship, L)), Lengths),
        sort(Lengths, SortedLengths),
        member(Length, SortedLengths),
        include(=(Length), Lengths, LengthOccurrences),
        length(LengthOccurrences, Count)
    ), GroupedShips).

get_complete_grid_data(JsonResponse) :-
    reload_facts,
    grid(Size),
    findall(Row,
        (between(1, Size, R),
         findall(CellData,
            (between(1, Size, C),
             cell(R, C, TypeNum, ShapeNum, DirNum),
             (TypeNum =:= 2 -> TypeAtom = ship ; TypeAtom = sea),
             CellData = _{type: TypeAtom, shape: ShapeNum, direction: DirNum}
            ), Row)
        ), GridMatrix),

    findall(_{required: Count}, 
        (between(1, Size, R), row(R, Count)), RowReqs),

    findall(_{required: Count}, 
        (between(1, Size, C), col(C, Count)), ColReqs),

    findall(_{length: Length, count: Count}, 
        ship(Length, Count), Ships),

    JsonResponse = _{
        size: Size,
        grid: GridMatrix,
        row_requirements: RowReqs,
        col_requirements: ColReqs,
        ships: Ships
    }.

reload_facts :-
    unload_file('facts.pl'),
    consult('facts.pl').