:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:- consult('task1.pl').


:- http_handler(root(static), http_reply_from_files('static', []), [prefix]).


:- http_handler(root(.), serve_index, []).

serve_index(Request) :-
    cors_enable_all(Request),
    http_reply_file('static/index.html', [], Request).

start :-
    http_server(http_dispatch, [port(8080), reuse_address(true)]).

:- initialization(start).