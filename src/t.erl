-module(t). % `t`ools module

-compile(export_all).
-export([interpose/2]).

day_changed() ->
    spawn_link(?MODULE, day_changed, [calendar:local_time()]).

day_changed(PrevDate) ->
    CurrentDate = {{_,_,Today}, _} = calendar:local_time(),
    case PrevDate of
        {{_,_,Today}, {_,_,_}} ->
            %% nothing to do here, we just wait a bit and check again later
            receive after 5000 -> day_changed(PrevDate) end;

        _ ->
            %% the day have changed!
            io:format("New day has arrived! It's ~p now", [CurrentDate]),
            day_changed(CurrentDate)
    end.




fmt(Term) ->
    io_lib:format("~p", [Term]).

fmtnl(Term) ->
    io_lib:format("~p~n", [Term]).


p(Term) ->
    io:format("~p", [Term]).

pnl(Term) ->
    io:format("~n~n~p~n~n", [Term]).


interpose(List, Sep) ->
    interpose(List, Sep, []).

interpose([]           , Sep, Acc) -> [];
interpose([Term]       , Sep, Acc) -> lists:reverse([Term | Acc]);
interpose([Head | Tail], Sep, Acc) -> interpose(Tail, Sep, [Sep, Head | Acc]).
