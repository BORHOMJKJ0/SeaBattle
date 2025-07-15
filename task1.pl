:- set_prolog_flag(encoding, utf8).
:- encoding(utf8).
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
        (
            reload_facts,
            get_complete_grid_data(Data),
            reply_json(Data)
        ),
        Error,
        (
            cors_enable_all(Request),
            reply_json(_{error: "Grid data error", details: Error})
        )
    ).
handle_grid(Request) :-
    cors_enable_all(Request),
    throw(http_reply(method_not_allowed([get, options]))).

handle_validate(Request) :-
    option(method(options), Request), !,
    cors_reply(Request).
handle_validate(Request) :-
    ( option(method(post), Request) ->
        cors_enable_all(Request),
        catch(
            (
                http_read_json_dict(Request, Dict),
                ( get_dict(grid, Dict, GridData) ->
                    validate_grid_with_details(GridData, ValidationResult),
                    reply_json_with_utf8(ValidationResult)
                ; throw(error(missing_grid_key, _))
                )
            ),
            Error,
            (
                reply_json_with_utf8(_{
                    valid: false,
                    message: "حدث خطأ أثناء التحقق",
                    error: Error
                })
            )
        )
    ; cors_enable_all(Request),
      throw(http_reply(method_not_allowed([post, options])))
    ).

reply_json_with_utf8(Data) :-
    cors_enable_all(_),
    reply_json_dict(Data, [json_object(dict)]).

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
