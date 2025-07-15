:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- consult('logic.pl').

:- http_handler(root(api/status), handle_status, []).
:- http_handler(root(api/grid), handle_grid, []).
:- http_handler(root(api/validate), handle_validate, []).

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
            (   validate_grid(GridData) ->
                (   format(user_error, 'DEBUG: Validation successful~n', []),
                    reply_json_with_utf8(_{valid: true, message: "\u0627\u0644\u062D\u0644 \u0635\u062D\u064A\u062D \u2705"})
                )
            ;   (   format(user_error, 'DEBUG: Validation failed~n', []),
                    reply_json_with_utf8(_{valid: false, message: "\u0627\u0644\u062D\u0644 \u062E\u0627\u0637\u0626 \u274C"})
                )
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