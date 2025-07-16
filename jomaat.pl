:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).
:- discontiguous grid_api/1.
:- discontiguous status_api/1.
:- dynamic cell/5.
:- dynamic grid/1.
:- dynamic row/2.
:- dynamic col/2.
:- dynamic ship/2.

shape(0, unknown).
shape(1, curve).
shape(2, square).
shape(3, circle).
shape(4, sea).

direction(0, dontCare).
direction(1, up).
direction(2, down).
direction(3, left).
direction(4, right).

type(1, sea).
type(2, ship).
type(3, empty).

grid(3).

row(1, 2).
row(2, 0).
row(3, 0).

col(1, 1).
col(2, 1).
col(3, 0).

ship(2, 1).

cell(1, 1, 2, 1, 1). 
cell(1, 2, 2, 1, 1). 
cell(1, 3, 1, 4, 0).  
cell(2, 1, 1, 4, 0).  
cell(2, 2, 1, 4, 0).  
cell(2, 3, 1, 4, 0). 
cell(3, 1, 1, 4, 0).  
cell(3, 2, 1, 4, 0).  
cell(3, 3, 1, 4, 0).  

neighbor_offsets([
    (-1, 0), (1, 0), (0, -1), (0, 1)
]).

find_all_ships(Ships) :-
    grid(_),
    findall((R,C), cell(R,C,2,_,_), ShipCells),
    find_connected_ships(ShipCells, [], Ships).

find_connected_ships([], _, []).
find_connected_ships([Cell|Rest], Visited, [Ship|Ships]) :-
    \+ member(Cell, Visited),
    build_connected_ship(Cell, [Cell], Ship),
    append(Visited, Ship, NewVisited),
    subtract(Rest, Ship, NewRest),
    find_connected_ships(NewRest, NewVisited, Ships).
find_connected_ships([Cell|Rest], Visited, Ships) :-
    member(Cell, Visited),
    find_connected_ships(Rest, Visited, Ships).

build_connected_ship(Cell, Visited, Ship) :-
    find_all_neighbors(Cell, Neighbors),
    extend_ship_with_neighbors(Neighbors, Visited, Ship).

find_all_neighbors((R,C), Neighbors) :-
    neighbor_offsets(Offsets),
    findall((NR, NC),
        ( member((DR,DC), Offsets),
          NR is R + DR, NC is C + DC,
          grid(Size), NR >= 1, NR =< Size, NC >= 1, NC =< Size,
          cell(NR, NC, 2, _, _)
        ),
        Neighbors
    ).

extend_ship_with_neighbors([], Ship, Ship).
extend_ship_with_neighbors([Neighbor|Rest], Visited, Ship) :-
    ( member(Neighbor, Visited) ->
        extend_ship_with_neighbors(Rest, Visited, Ship)
    ;   append(Visited, [Neighbor], NewVisited),
        build_connected_ship(Neighbor, NewVisited, TempShip),
        extend_ship_with_neighbors(Rest, TempShip, Ship)
    ).

update_ship_shapes :-
    find_all_ships(Ships),
    forall(member(Ship, Ships), (
        length(Ship, Len),
        ( Len =:= 1 ->
            Ship = [(R,C)],
            cell(R,C,2,_,D),
            retract(cell(R,C,2,_,D)),
            assert(cell(R,C,2,3,D)) 
        ; 
          Ship = [(R1,C1)|Rest],
          last(Rest, (RLast, CLast)),
          cell(R1,C1,2,_,D1),
          retract(cell(R1,C1,2,_,D1)),
          assert(cell(R1,C1,2,1,D1)), 
          cell(RLast, CLast, 2, _, DLast),
          retract(cell(RLast, CLast, 2, _, DLast)),
          assert(cell(RLast, CLast, 2, 1, DLast)),  
          forall(
            (member((R,C), Rest), (R,C) \= (RLast, CLast)),
            (
                cell(R,C,2,_,D),
                retract(cell(R,C,2,_,D)),
                assert(cell(R,C,2,2,D))  
            )
          )
        )
    )).


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

validate_no_side_touching :-
    find_all_ships(Ships),
    \+ (
        member(Ship1, Ships),
        member(Ship2, Ships),
        Ship1 \= Ship2,
        ships_side_touching(Ship1, Ship2)
    ).

ships_side_touching(Ship1, Ship2) :-
    member((R1,C1), Ship1),
    member((R2,C2), Ship2),
    (
        (R1 =:= R2, abs(C1 - C2) =:= 1) ;
        (C1 =:= C2, abs(R1 - R2) =:= 1)
    ).

validate_no_diagonal_touching :-
    find_all_ships(Ships),
    \+ (
        member(Ship1, Ships),
        member(Ship2, Ships),
        Ship1 \= Ship2,
        ships_diagonal_touching(Ship1, Ship2)
    ).

ships_diagonal_touching(Ship1, Ship2) :-
    member((R1,C1), Ship1),
    member((R2,C2), Ship2),
    abs(R1 - R2) =:= 1,
    abs(C1 - C2) =:= 1.

validate_ship_shapes :-
    find_all_ships(Ships),
    forall(member(Ship, Ships), (
        valid_ship_shape(Ship),
        valid_ship_shape_pattern(Ship)
    )).

valid_ship_shape(Ship) :-
    maplist(arg(1), Ship, Rows),
    maplist(arg(2), Ship, Cols),
    ( all_equal(Rows) -> consecutive(Cols)
    ; all_equal(Cols) -> consecutive(Rows)
    ).

all_equal([_]).
all_equal([X,Y|Rest]) :-
    X =:= Y,
    all_equal([Y|Rest]).

consecutive(List) :-
    sort(List, Sorted),
    Sorted = [Min|_],
    last(Sorted, Max),
    length(Sorted, Len),
    Max - Min + 1 =:= Len.

valid_ship_shape_pattern(Ship) :-
    length(Ship, Len),
    ( Len =:= 1 -> 
        Ship = [(R,C)],
        cell(R,C,2,3,_)  
    ; Len =:= 2 ->
        forall(member((R,C), Ship), cell(R,C,2,1,_)) 
    ; Len > 2 ->
        Ship = [(R1,C1)|Rest],
        last(Rest, (RLast, CLast)),
        cell(R1,C1,2,1,_), 
        cell(RLast, CLast, 2, 1, _),  
        forall(
            (member((R,C), Rest), (R,C) \= (RLast, CLast)),
            cell(R,C,2,2,_)  
        )
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

validate_complete_solution(Result) :-
    check_validation_rule_all(Errors),
    ( Errors == [] ->
        Result = valid(true, "الحل صحيح", [])
    ; Result = valid(false, "الحل خاطئ", Errors)
    ).

check_validation_rule_all(Errors) :-
    findall(Error,
        (
          ( \+ validate_row_counts -> Error = "أعداد السفن في الصفوف غير صحيحة" ; fail );
          ( \+ validate_col_counts -> Error = "أعداد السفن في الأعمدة غير صحيحة" ; fail );
          ( \+ validate_no_side_touching -> Error = "ممنوع تلامس السفن جانبياً" ; fail );
          ( \+ validate_no_diagonal_touching -> Error = "ممنوع تلامس السفن قطرياً" ; fail );
          ( \+ validate_ship_shapes -> Error = "شكل السفن غير صحيح" ; fail );
          ( \+ validate_ship_counts -> Error = "أعداد السفن حسب الطول غير صحيحة" ; fail )
        ),
        Errors).

get_cell_info(R, C, Type, Shape, Direction) :-
    cell(R, C, Type, Shape, Direction).

get_ships_with_lengths(ShipsWithLengths) :-
    find_all_ships(Ships),
    findall(Ship-Length, (member(Ship, Ships), length(Ship, Length)), ShipsWithLengths).

is_valid_position(R, C) :-
    grid(Size),
    R >= 1, R =< Size,
    C >= 1, C =< Size.