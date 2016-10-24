-module(reqviewer_handler_history).

% -compile([{parse_transform, lager_transform}]).
-export([init/2]).


init(Req, Opts) ->
    Hist = queue:to_list(reqviewer_redis_sub:history()),
    t:p(Hist),
    Body = ["[", t:interpose(Hist, ","), "]"],
    Req2 = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, Opts}.
