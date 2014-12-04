-module(t). % `t`ools module

-compile(export_all).

fmt(Term) ->
    io_lib:format("~p", [Term]).
