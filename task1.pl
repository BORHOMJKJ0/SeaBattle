:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- consult('logic.pl').

:- http_handler(root(api/status), handle_status, []).
:- http_handler(root(api/grid), handle_grid, []).
:- http_handler(root(api/validate), handle_validate, []).
:- encoding(utf8).

handle_status(Request) :-
    option(method(options), Request), !,
    cors_reply(Request).
handle_status(Request) :-
    option(method(get), Request), !,
    cors_enable_all(Request),
    reply_json(_{status: "online"}).
handle_status(Request) :-
    cors_enable_all(Request),
    throw(http_reply(method_not_allowed([get, options]))).

handle_grid(Request) :-
    option(method(options), Request), !,
    cors_reply(Request).
handle_grid(Request) :-
    option(method(get), Request), !,
    cors_enable_all(Request),
    catch(
        (get_complete_grid_data(Data), 
         reply_json(Data)),
        Error,
        (format('Content-Type: application/json; charset=UTF-8~n~n'),
         format('{"error": "Grid data error", "details": "~w"}~n', [Error]))
    ).
handle_grid(Request) :-
    cors_enable_all(Request),
    throw(http_reply(method_not_allowed([get, options]))).

handle_validate(Request) :-
    option(method(options), Request), !,
    cors_reply(Request).
handle_validate(Request) :-
    option(method(post), Request), !,
    cors_enable_all(Request),
    catch(
        (
            format(user_error, 'DEBUG: Starting validation~n', []),
            http_read_json_dict(Request, Dict),
            format(user_error, 'DEBUG: JSON read successfully: ~w~n', [Dict]),
            
            (   get_dict(grid, Dict, GridData) ->
                format(user_error, 'DEBUG: Grid data found: ~w~n', [GridData])
            ;   format(user_error, 'DEBUG: No grid key in JSON~n', []),
                throw(no_grid_data)
            ),
            
            format(user_error, 'DEBUG: Attempting validation~n', []),
            (   validate_grid_with_details(GridData, ValidationResult) ->
                format(user_error, 'DEBUG: Validation completed~n', []),
                reply_json_with_utf8(ValidationResult)
            ;   format(user_error, 'DEBUG: Validation failed with unknown error~n', []),
                reply_json_with_utf8(_{
                    valid: false, 
                    message: "خطأ غير معروف في التحقق",
                    errors: ["فشل التحقق لسبب غير معروف"]
                })
            )
        ),
        Error,
        (
            format(user_error, 'DEBUG: Caught error: ~w~n', [Error]),
            cors_enable_all(Request),
            format('Content-Type: application/json; charset=UTF-8~n~n'),
            format('{"valid": false, "message": "خطأ في الخادم", "error": "~w"}~n', [Error])
        )
    ).
handle_validate(Request) :-
    cors_enable_all(Request),
    throw(http_reply(method_not_allowed([post, options]))).

validate_grid_with_details(GridData, Result) :-
    update_grid_from_json(GridData),
    findall(Error, check_validation_rule(Error), Errors),
    (   Errors = [] ->
        Result = _{valid: true, message:  "\u0627\u0644\u062D\u0644 \u0635\u062D\u064A\u062D \u2705", errors: []}
    ;   Result = _{valid: false, message:  "\u0627\u0644\u062D\u0644 \u062E\u0627\u0637\u0626 \u274C", errors: Errors}
    ).
print_grid :-
    findall(cell(R,C,T), cell(R,C,T,_,_), Cells),
    max_row_col(Cells, MaxR, MaxC),
    print_rows(1, MaxR, MaxC).

max_row_col(Cells, MaxR, MaxC) :-
    findall(R, member(cell(R,_,_), Cells), Rs),
    findall(C, member(cell(_,C,_), Cells), Cs),
    max_list(Rs, MaxR),
    max_list(Cs, MaxC).

print_rows(R, MaxR, MaxC) :-
    R =< MaxR,
    print_row(R, 1, MaxC),
    nl,
    R1 is R + 1,
    print_rows(R1, MaxR, MaxC).
print_rows(R, MaxR, _) :-
    R > MaxR.

print_row(_, C, MaxC) :-
    C > MaxC.
print_row(R, C, MaxC) :-
    C =< MaxC,
    ( cell(R, C, ship, _, _) -> write('S') ; write('~') ),
    write(' '),
    C1 is C + 1,
    print_row(R, C1, MaxC).

check_validation_rule(Error) :-
    \+ validate_row_counts,
    Error = "أعداد السفن في الصفوف غير صحيحة".

check_validation_rule(Error) :-
    \+ validate_col_counts,
    Error = "أعداد السفن في الأعمدة غير صحيحة".

check_validation_rule(Error) :-
    \+ validate_no_diagonal_touching,
    Error = "السفن تتلاقى قطرياً - هذا غير مسموح".

check_validation_rule(Error) :-
    \+ validate_ship_shapes,
    Error = "شكل السفن غير صحيح - يجب أن تكون مستقيمة".

check_validation_rule(Error) :-
    \+ validate_ship_counts,
    Error = "أعداد السفن حسب الطول غير صحيحة".

validate_row_counts_detailed(Errors) :-
    grid(MaxRows),
    findall(Error, (
        between(1, MaxRows, R),
        row(R, Expected),
        count_ships_in_row(R, Actual),
        Expected =\= Actual,
        format(atom(Error), 'الصف ~w: متوقع ~w سفن، موجود ~w', [R, Expected, Actual])
    ), Errors).

validate_col_counts_detailed(Errors) :-
    grid(MaxCols),
    findall(Error, (
        between(1, MaxCols, C),
        col(C, Expected),
        count_ships_in_col(C, Actual),
        Expected =\= Actual,
        format(atom(Error), 'العمود ~w: متوقع ~w سفن، موجود ~w', [C, Expected, Actual])
    ), Errors).

reply_json_with_utf8(Data) :-
    format('Content-Type: application/json; charset=UTF-8~n~n'),
    json_write(current_output, Data).

cors_reply(Request) :-
    cors_enable_all(Request),
    format('Status: 200 OK~n'),
    format('Content-Type: text/plain; charset=UTF-8~n'),
    format('Content-Length: 0~n~n').

cors_enable_all(Request) :-
    cors_enable(Request, [
        methods([get, post, options]),
        headers(['Content-Type', 'Authorization']),
        credentials(false)
    ]).