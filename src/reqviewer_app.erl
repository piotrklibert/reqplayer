-module(reqviewer_app).
-compile([{parse_transform, lager_transform}]).


-behaviour(application).
-export([start/2, stop/1]).


-export([start/0]).


%% ===================================================================
%% External API
%% ===================================================================

start() ->
    ok = application:start(reqviewer).



%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    {ok, ServerPort} = application:get_env(reqviewer, viewer_port),

    try
        ok = application:start(ranch),
        ok = application:start(crypto),
        ok = application:start(cowlib),
        ok = application:start(cowboy)
    catch
        Cls:Error ->
            io:format("A prerequisite module ~p not found", [Error])
    end,

    lager:start(),

    {ok, _} = reqviewer_sup:start_link(self()),


    RoutesTable = cowboy_router:compile(
        [{'_', [
            {"/websocket", reqviewer_handler, []},
            {"/history", reqviewer_handler_history, []},
            {"/suites", reqviewer_testsuites, []},
            {"/", cowboy_static, {priv_file, reqviewer, "html/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, reqviewer, ""}}
        ]}]
    ),

    {ok, _} = cowboy:start_http(
        http, 15, [{port, ServerPort}],
        [{env, [{dispatch, RoutesTable}]}]
    ).


stop(_State) ->
    ok.
