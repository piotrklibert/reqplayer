-module(t). % `t`ools module

-compile(export_all).

fmt(Term) ->
    io_lib:format("~p", [Term]).

fmtnl(Term) ->
    io_lib:format("~p~n", [Term]).


p(Term) ->
    io:format("~p", [Term]).

pnl(Term) ->
    io:format("~n~n~p~n~n", [Term]).
