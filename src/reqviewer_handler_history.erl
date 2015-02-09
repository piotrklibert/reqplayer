-module(reqviewer_handler_history).

% -compile([{parse_transform, lager_transform}]).
-export([init/2]).
-export([interpose/2]).



init(Req, Opts) ->
    Hist = queue:to_list(reqviewer_redis_sub:history()),
    io:format("~p", [Hist]),
    Body = ["[", interpose(Hist, ","), "]"],
    Req2 = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, Opts}.



interpose(List, Sep) ->
    interpose(List, Sep, []).

interpose([]           , Sep, Acc) -> [];
interpose([Term]       , Sep, Acc) -> lists:reverse([Term | Acc]);
interpose([Head | Tail], Sep, Acc) -> interpose(Tail, Sep, [", ", Head | Acc]).
