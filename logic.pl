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
    validate_col_counts.

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

count_ships_in_row(R, Count) :-
    grid(MaxCols),
    findall(1, (between(1, MaxCols, C), cell(R, C, ship, _, _)), L),
    length(L, Count).

count_ships_in_col(C, Count) :-
    grid(MaxRows),
    findall(1, (between(1, MaxRows, R), cell(R, C, ship, _, _)), L),
    length(L, Count).


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
