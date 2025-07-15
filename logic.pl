:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).
:- consult('facts.pl').
:- use_module(library(http/json)).

:- dynamic grid/1.  % هذا التعريف يسمح بتغيير grid/1 أثناء التشغيل

neighbor_offsets([
    (-1, 0), (1, 0), (0, -1), (0, 1),
    (-1, -1), (-1, 1), (1, -1), (1, 1)
]).

validate_grid_with_details(GridData, Result) :-
    update_grid_from_json(GridData),
    findall(Error, check_validation_rule(Error), Errors),
    ( Errors == [] ->
        Result = _{valid: true, message: "الحل صحيح ✅", errors: []}
    ; Result = _{valid: false, message: "الحل خاطئ ❌", errors: Errors}
    ).

check_validation_rule("أعداد السفن في الصفوف غير صحيحة") :- \+ validate_row_counts.
check_validation_rule("أعداد السفن في الأعمدة غير صحيحة") :- \+ validate_col_counts.
check_validation_rule("السفن لا يجب أن تلمس بعضها حتى قطرياً") :- \+ validate_no_touching_ships.
check_validation_rule("شكل السفن غير صحيح - يجب أن تكون مستقيمة") :- \+ validate_ship_shapes.
check_validation_rule("أعداد السفن حسب الطول غير صحيحة") :- \+ validate_ship_counts.

update_grid_from_json(GridData) :-
    retractall(cell(_, _, _, _, _)),
    retractall(grid(_)),
    length(GridData, N),
    assert(grid(N)),
    assert_grid_from_json(GridData, 1).

assert_grid_from_json([], _).
assert_grid_from_json([Row|Rest], R) :-
    assert_row_from_json(Row, R, 1),
    R1 is R + 1,
    assert_grid_from_json(Rest, R1).

assert_row_from_json([], _, _).
assert_row_from_json([Cell|Rest], R, C) :-
    (Cell == 2 -> Type = 2 ; Type = 1),
    assert(cell(R, C, Type, nil, nil)),
    C1 is C + 1,
    assert_row_from_json(Rest, R, C1).

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
    findall(1, (between(1, MaxCols, C), cell(R, C, 2, _, _)), L),
    length(L, Count).

count_ships_in_col(C, Count) :-
    grid(MaxRows),
    findall(1, (between(1, MaxRows, R), cell(R, C, 2, _, _)), L),
    length(L, Count).

validate_no_touching_ships :-
    find_all_ships(Ships),
    \+ (
        member(Ship1, Ships),
        member(Ship2, Ships),
        Ship1 \= Ship2,
        ships_touching(Ship1, Ship2)
    ).

ships_touching(Ship1, Ship2) :-
    member((R1,C1), Ship1),
    member((R2,C2), Ship2),
    DR is abs(R1 - R2), DC is abs(C1 - C2),
    DR =< 1, DC =< 1,
    (DR > 0 ; DC > 0).

validate_ship_shapes :-
    find_all_ships(Ships),
    forall(member(Ship, Ships), valid_ship_shape(Ship)).

valid_ship_shape(Ship) :-
    maplist(arg(1), Ship, Rows),
    maplist(arg(2), Ship, Cols),
    ( all_equal(Rows) -> consecutive(Cols)
    ; all_equal(Cols) -> consecutive(Rows)
    ).

all_equal([_]).
all_equal([X,Y|Rest]) :- X =:= Y, all_equal([Y|Rest]).

consecutive(List) :-
    sort(List, Sorted),
    Sorted = [Min|_],
    last(Sorted, Max),
    length(Sorted, Len),
    Max - Min + 1 =:= Len.

find_all_ships(Ships) :-
    grid(_),
    findall((R,C), cell(R,C,2,_,_), ShipCells),
    find_all_ships(ShipCells, [], Ships).

find_all_ships([], _, []).
find_all_ships([H|Rest], Visited, [Ship|Ships]) :-
    \+ member(H, Visited),
    build_ship_from(H, [H], Ship),
    append(Visited, Ship, NewVisited),
    subtract(Rest, Ship, NewRest),
    find_all_ships(NewRest, NewVisited, Ships).
find_all_ships([H|Rest], Visited, Ships) :-
    member(H, Visited), find_all_ships(Rest, Visited, Ships).

build_ship_from((R,C), Acc, Ship) :-
    neighbor_offsets(Offsets),
    findall((NR,NC),
        ( member((DR,DC), Offsets),
          NR is R + DR, NC is C + DC,
          grid(Size), NR >=1, NR =< Size, NC >=1, NC =< Size,
          cell(NR, NC, 2, _, _),
          \+ member((NR,NC), Acc)
        ), Neighbors),
    ( Neighbors = [] -> Ship = Acc
    ; Neighbors = [Next|_],
      build_ship_from(Next, [Next|Acc], Ship)
    ).

validate_ship_counts :-
    find_all_ships(Ships),
    group_ships_by_length(Ships, Grouped),
    forall(ship(Len, Count), (
        ( member(Len-Actual, Grouped) -> Actual =:= Count ; Count =:= 0 )
    )).

group_ships_by_length(Ships, Grouped) :-
    findall(Len, (member(S, Ships), length(S, Len)), Lengths),
    sort(Lengths, Unique),
    findall(Len-Count, (
        member(Len, Unique),
        include(=(Len), Lengths, Matches),
        length(Matches, Count)
    ), Grouped).

reload_facts :-
    unload_file('facts.pl'),
    catch(consult('facts.pl'), E, print_message(error, E)).

get_complete_grid_data(Json) :-
    reload_facts,
    grid(Size),
    findall(Row, (between(1, Size, R),
        findall(Cell, (between(1, Size, C),
            cell(R, C, T, S, D),
            (T=2 -> Type=ship; Type=sea),
            Cell=_{type:Type, shape:S, direction:D}
        ), Row)
    ), Grid),
    findall(_{required:Count}, (between(1, Size, R), row(R, Count)), Rows),
    findall(_{required:Count}, (between(1, Size, C), col(C, Count)), Cols),
    findall(_{length:L, count:C}, ship(L,C), Ships),
    Json = _{size:Size, grid:Grid, row_requirements:Rows, col_requirements:Cols, ships:Ships}.
